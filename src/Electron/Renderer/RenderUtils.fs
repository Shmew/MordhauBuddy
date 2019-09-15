namespace MordhauBuddy.App

/// Helper types for sending bridge messages
module BridgeUtils =
    open MordhauBuddy.Shared.ElectronBridge

    /// Send community operations
    type CommunityBridgeSender(caller: Caller) =
        let wrapComs cCmd = BridgeOps(CommunityOperation(cCmd), caller)
        member this.GetSteamAnnouncements() = CommunityOperation.GetSteamAnnouncements |> wrapComs

    /// Send INI operations
    type INIBridgeSender(caller: Caller) =
        let wrapOps iCmd = BridgeOps(INIOperation(iCmd), caller)
        let wrapFace fCmd = BridgeOps(Faces(fCmd), caller)
        let wrapConf cCmd = BridgeOps(Configs(cCmd), caller)
        /// Try to locate the default Mordhau configuration directory
        member this.DefaultDir = INIFileOperation.DefaultDir |> wrapOps
        /// Replace the original value with new value based on selectors
        member this.Replace s sels iFile = INIFileOperation.Replace(s, sels, iFile) |> wrapOps
        /// Delete contents that match selectors
        member this.Delete sels = INIFileOperation.Delete(sels) |> wrapOps
        /// Determine if a file exists
        member this.Exists iFile = INIFileOperation.Exists(iFile) |> wrapOps
        /// Try to open and parse a ini file
        member this.Parse iFile = INIFileOperation.Parse(iFile) |> wrapOps
        /// Create a backup of a file by putting it into a subdirectory
        member this.Backup iList = INIFileOperation.Backup(iList) |> wrapOps
        /// Push changes to the configuration file(s)
        member this.Commit iList = INIFileOperation.Commit(iList) |> wrapOps
        /// Set random face values to given profiles
        member this.SetRandom profiles = Random(profiles) |> wrapFace
        /// Set max and min face values to given profiles
        member this.SetFrankenstein profiles = Frankenstein(profiles) |> wrapFace
        /// Set face values to given profiles
        member this.SetCustom profiles fVal = Custom(profiles, fVal) |> wrapFace
        /// Get the current profiles
        member this.GetProfileList = ProfileList |> wrapFace
        /// Get the current configurations
        member this.GetConfigs oList = GetConfigs(oList) |> wrapConf
        /// Modify the configurations based on changes
        member this.MapConfigs oList = MapConfigs(oList) |> wrapConf

    /// Send setting operations
    type SettingBridgeSender(caller: Caller) =
        let wrapSetting sCmd = BridgeOps(SettingsOperation(sCmd), caller)
        /// Enable launch at startup
        member this.EnableAutoLaunch = SettingsOperation.EnableAutoLaunch |> wrapSetting
        /// Disable launch at startup
        member this.DisableAutoLaunch = SettingsOperation.DisableAutoLaunch |> wrapSetting
        /// Modify the backup policy
        member this.BackupPolicy bSet = SettingsOperation.BackupPolicy(bSet) |> wrapSetting
        /// Setup initial Linux configuration
        member this.SetupLinux = SettingsOperation.SetupLinux |> wrapSetting

    /// Send app operations
    type AppBridgeSender(caller: Caller) =
        let wrapUpdate uCmd = BridgeOps(Updates(uCmd), caller)
        let wrapMisc mCmd = BridgeOps(Misc(mCmd), caller)
        /// Begin patching new update
        member this.StartUpdate = Updates.Start |> wrapUpdate
        /// Check for new updates
        member this.CheckUpdate = Updates.Check |> wrapUpdate
        /// Check if Mordhau is running
        member this.CheckMordhau = MiscOperation.IsMordhauRunning |> wrapMisc

/// Helper modules and functions for renderer processes
module RenderUtils =
    open Electron
    open Fable.Core
    open Fable.Core.JsInterop
    open Fable.Import
    open Browser.Types
    open Node.Api
    open MordhauBuddy.Shared.ElectronBridge

    /// Returns `true` if the environment is Windows
    let isWindows = Node.Api.``process``.platform = Node.Base.Platform.Win32

    /// Get the current window from the `main` process
    let getRemoteWin() = renderer.remote.getCurrentWindow()

    /// Persist an event
    [<Emit("$0.persist()")>]
    let eventPersist (e: Event): unit = jsNative

    /// Get the closest element at given position
    [<Emit("try{document.elementFromPoint($0, $1)}catch(e){}")>]
    let getElementAtPos (x: int) (y: int): HTMLElement option = jsNative

    /// Get the current mouse position
    let getMousePositions() =
        let absMouse = renderer.remote.screen.getCursorScreenPoint()
        let absWindow = renderer.remote.getCurrentWindow().getBounds()
        {| X = absMouse.x - absWindow.x
           Y = absMouse.y - absWindow.y |}

    /// Prefixes the string with the static asset root path.
    let stat (s: string) =
#if DEBUG
        s
#else
        path.resolve (__dirname, "..", "..", "static", s)

#endif

    /// Exit the application
    let quitApplication() = Electron.renderer.remote.app.quit()

    [<AutoOpen>]
    module RenderTypes =
        open FSharp.Reflection

        [<RequireQualifiedAccess>]
        type Submit =
            | Waiting
            | Init
            | Error of string
            | Success of string

            member this.IsSubmitWaiting =
                match this with
                | Submit.Waiting -> true
                | _ -> false

            member this.IsSubmitInit =
                match this with
                | Submit.Init -> true
                | _ -> false

            member this.IsSubmitError =
                match this with
                | Submit.Error _ -> true
                | _ -> false

            member this.IsSubmitSuccess =
                match this with
                | Submit.Success _ -> true
                | _ -> false

    module String =
        open System.Text.RegularExpressions

        /// Ensures that the string ends with a given suffix
        let ensureEndsWith (suffix: string) (str: string) =
            if str.EndsWith suffix then str
            else str + suffix

        /// Converts a disciminated union string to a title
        let duToTitle (s: string) =
            MatchEvaluator(fun m -> " " + m.Value)
            |> (fun m -> Regex.Replace(s.Substring(1), "[A-Z]", m))
            |> (+) (s.Substring(0, 1))

        /// Default value for string option into `str`
        let defStr (sOpt: string option) = Fable.React.Helpers.str (defaultArg sOpt "")

    [<AutoOpen>]
    module Extensions =
        type Result<'T, 'TError> with

            member this.IsOk =
                match this with
                | Ok _ -> true
                | Error _ -> false

            member this.IsError =
                match this with
                | Error _ -> true
                | Ok _ -> false

            member this.ErrorOr value =
                match this with
                | Ok _ -> value
                | Error err -> err

        let errorStrings (res: Result<string, string list>) =
            match res with
            | Ok(s: string) -> s
            | Error(err: string list) -> err |> List.reduce (fun acc elem -> acc + " " + elem)

    /// Input validation
    module Validation =
        open System.Text.RegularExpressions

        /// Regular expression constants
        module RegPatterns =
            let translate = @"^\(Translate=\((\d*,?){49}\),Rotate=\((\d*,?){49}\),Scale=\((\d*,?){49}\)\)"
            let fileSize = @"^(\d+[,.]\d+|\d+)\s*(kb|mb|gb)"
            let semVersion = @"^(\d+)[. ,]*(\d+)*[. ,]*(\d)*"
            let playersStatic = @"^(\d+)\w*.\w*\D+$"
            let playersRange = @"^(\d+).*?(\d+)"
            let gDrive =
                @"^.*drive.google.com\/open\?id=(.*)$|^.*drive.google.com\/file\/d\/(.*)\/view$|^.*drive.google.com\/uc\?id=(.*)&export=download$"
            let imgur = @"https://imgur.com"
            let imgurExt = @"https://i.imgur.com/\w+$"
            let gdImgBad = @"(https://drive.google.com/open\?id=.+$)"
            let gdImgId = @"id=.+$"

        [<AutoOpen>]
        module private Helpers =
            open System

            /// Remove all whitespace
            let removeAllWs (s: string) =
                s.Trim().ToCharArray()
                |> Array.choose (fun c ->
                    if c = ' ' then None
                    else Some(string c))
                |> Array.reduce (+)

            /// Apply a pattern and if there is a match apply f
            let applyRPattern (pattern: string) (input: string) f =
                try
                    Regex(pattern, RegexOptions.IgnoreCase).Match(input)
                    |> fun m ->
                        seq {
                            for items in m.Groups do
                                yield items
                        }
                    |> List.ofSeq
                    |> function
                    | sList when sList.Length = 1 -> sList
                    | sList -> sList |> List.tail
                    |> List.map (fun g -> g.Value |> f)
                with _ -> []

            /// Map input string to file size
            let getFileSize (s: string) =
                let matchSize = applyRPattern RegPatterns.fileSize (removeAllWs s) (fun s -> s.Replace(",", "."))

                let convUnit (s: string) (f: float) =
                    match s.ToLower() with
                    | "kb" ->
                        1.0<KB> * f
                        |> convertKBtoMB
                        |> Some
                    | "mb" -> 1.0<MB> * f |> Some
                    | "gb" ->
                        1.0<GB> * f
                        |> convertGBtoMB
                        |> Some
                    | _ -> None

                match matchSize with
                | [ num; sizeUnit ] ->
                    match Double.TryParse num with
                    | (true, f) -> Some(f)
                    | _ -> None
                    |> Option.bind (convUnit sizeUnit)
                | _ -> None

            let private getInt (s: string) =
                match Int32.TryParse s with
                | (true, i) -> i
                | _ -> 0

            let private tryGetInt (s: string) =
                match Int32.TryParse s with
                | (true, i) -> i |> Some
                | _ -> None

            /// Map string to `MapVersion`
            let getVer (vStr: string) =
                applyRPattern RegPatterns.semVersion vStr getInt
                |> fun iList ->
                    match iList.Length with
                    | i when i >= 3 -> iList |> List.take 3
                    | i -> List.init (3 - i) (fun _ -> 0) |> List.append iList
                    |> fun iL ->
                        {| Major = iL.[0]
                           Minor = iL.[1]
                           Patch = iL.[2] |}

            let getDate (s: string) =
                s.Split('/')
                |> fun sArr -> sprintf "%s/%s/%s" sArr.[1] sArr.[0] sArr.[2]
                |> fun s ->
                    match DateTime.TryParse(s) with
                    | (true, dt) -> dt |> Some
                    | _ -> None

            let getGDriveID (s: string) =
                applyRPattern RegPatterns.gDrive s id
                |> List.filter (fun s -> s <> "")
                |> List.tryHead

            /// Fix imgur links that lack fully qualified paths
            let fixImgur (s: string) =
                Regex(RegPatterns.imgur, RegexOptions.IgnoreCase).Replace(s, @"https://i.imgur.com")
                |> fun s -> Regex(RegPatterns.imgurExt, RegexOptions.IgnoreCase).Replace(s, @"$&.png")

            /// Fix Google Drive links that don't properly export images
            let fixGDImg (s: string) =
                if Regex(RegPatterns.gdImgBad, RegexOptions.IgnoreCase).IsMatch(s) then
                    applyRPattern RegPatterns.gdImgId s id
                    |> List.tryHead
                    |> Option.map
                        (fun s -> sprintf "%s%s%s" @"https://drive.google.com/uc?authuser=0&" s @"&export=download")
                    |> defaultArg
                    <| s
                else
                    s

        /// Validate a directory
        let validateDir (s: string) =
            Fable.Validation.Core.single <| fun t ->
                t.TestOne s
                |> t.IsValid (fun _ ->
                    try
                        s
                        |> path.normalize
                        |> path.parse
                        |> ignore
                        true
                    with _ -> false) "Invalid path"
                |> t.Trim
                |> t.NotBlank "Directory cannot be blank"
                |> t.End

        /// Validate profile import string
        let validateImport (s: string) =
            Fable.Validation.Core.single <| fun t ->
                t.TestOne s
                |> t.Trim
                |> t.NotBlank "Must provide import string"
                |> t.Match (Regex RegPatterns.translate) "Invalid import string"
                |> t.End

        /// Validate image
        let validateImg (s: string) =
            Fable.Validation.Core.single <| fun t ->
                t.TestOne s
                |> t.Trim
                |> t.NotBlank ""
                |> t.IsUrl s
                |> t.Map fixImgur
                |> t.Map fixGDImg
                |> t.End

        /// Validate Google Drive link
        let validateGDrive (s: string) =
            Fable.Validation.Core.single <| fun t ->
                t.TestOne s
                |> t.Trim
                |> t.NotBlank ""
                |> t.IsUrl s
                |> t.Map getGDriveID
                |> t.IsSome ""
                |> t.End

        /// Validate version
        let validateVer (s: string) =
            Fable.Validation.Core.single <| fun t ->
                t.TestOne s
                |> t.Trim
                |> t.Map getVer
                |> t.End

        /// Validate file size
        let validateFileSize (s: string) =
            Fable.Validation.Core.single <| fun t ->
                t.TestOne s
                |> t.Trim
                |> t.NotBlank ""
                |> t.Map getFileSize
                |> t.IsSome ""
                |> t.End

        /// Validate date
        let validateDate (s: string) =
            Fable.Validation.Core.single <| fun t ->
                t.TestOne s
                |> t.Trim
                |> t.NotBlank ""
                |> t.Map getDate
                |> t.IsSome ""
                |> t.End

        /// Validate any string by trimming, and ensure is not empty
        let validateGeneric (s: string) =
            Fable.Validation.Core.single <| fun t ->
                t.TestOne s
                |> t.Trim
                |> t.NotBlank ""
                |> t.End

    /// Directory helper functions
    module Directory =
        /// A type to represent the current directory state
        [<RequireQualifiedAccess>]
        type DirState =
            | Waiting
            | Init of string
            | Error of string
            | Success of string

            member this.IsDirWaiting =
                match this with
                | DirState.Waiting -> true
                | _ -> false

            member this.IsDirInit =
                match this with
                | DirState.Init _ -> true
                | _ -> false

            member this.IsDirError =
                match this with
                | DirState.Error _ -> true
                | _ -> false

            member this.IsDirSuccess =
                match this with
                | DirState.Success _ -> true
                | _ -> false

        type ConfigDir =
            { Dir: ConfigFile
              Directory: string
              Label: string
              State: DirState }

        [<RequireQualifiedAccess>]
        type DirSelect =
            | Selected of string
            | Canceled

        /// Create file browser
        let selectDir() =
            promise {
                let opts =
                    jsOptions<OpenDialogOptions> (fun o ->
                        /// See https://github.com/electron/electron/blob/master/docs/api/dialog.md
                        o.title <- "Select Mordhau Configuration Directory"
                        o.defaultPath <- renderer.remote.app.getPath AppPathName.Home
                        o.properties <- [| DialogFeature.OpenDirectory |])
                let! res = renderer.remote.dialog.showOpenDialog opts
                if res.canceled then
                    return DirSelect.Canceled
                else
                    return res.filePaths
                           |> Array.head
                           |> DirSelect.Selected
            }

        [<AutoOpen>]
        module Helpers =
            type Dirs =
                | Game
                | Engine
                | GameUser

            let setDirError s (dir: ConfigDir) = { dir with State = DirState.Error s }

            let setDirSuccess s (dir: ConfigDir) = { dir with State = DirState.Success s }

            let setDirInit s (dir: ConfigDir) = { dir with State = DirState.Init s }

            let setDirDirectory s (dir: ConfigDir) = { dir with Directory = s }

            let iFileWithDir (cFile: ConfigFile) (dir: string) =
                { File = cFile
                  WorkingDir = dir |> Some }

    /// Sample data
    module Samples =
        let faceImport = "(Translate=(65535,31072,875,704,0,734,65535,0,0,65535,31565,0,0,65535,0,29632,29662,30686,\
            65535,65535,0,30720,65535,0,0,918,31560,0,65535,31709,31680,544,574,30749,30720,256,286,65535,\
            0,0,0,65535,0,0,65535,0,65535,31678,31648),Rotate=(0,65535,26302,31680,0,30750,0,65535,0,0,0,\
            65535,65535,65535,31584,30749,31648,8,65535,0,65535,608,65535,65535,0,31695,893,18301,65535,31677,\
            30720,31704,30725,1,988,29,960,0,65535,0,65535,65535,16326,0,65535,65535,15383,30,960),Scale=(0,30,\
            4139,30749,65535,30749,0,65535,65535,0,0,0,0,65535,31709,0,0,190,0,0,0,589,0,0,0,30749,31166,989,\
            65535,5085,5085,4242,4242,0,0,24452,24452,65535,0,0,65535,65535,574,0,0,65535,574,21470,21470))"

        let typicalConfigDir = @"C:\Users\shmew\AppData\Local\Mordhau\Saved\Config\WindowsClient"

    /// Parsing rss feeds that return html
    module HtmlParsing =
        open System.Text.RegularExpressions

        let htmlClass = "(class=\".*?\")"
        let htmlImg = "<img "
        let htmlA = "<a "
        let steamLinkFilter = @"https://steamcommunity.com/linkfilter/?url="

        let stripClasses (s: string) = Regex(htmlClass, RegexOptions.IgnoreCase).Replace(s, "")

        let private addImgClasses (newClass: string) (s: string) =
            Regex(htmlImg, RegexOptions.IgnoreCase).Replace(s, sprintf "$& class=\"%s\"" (newClass + "Img"))

        let private addAClasses (newClass: string) (s: string) =
            Regex(htmlA, RegexOptions.IgnoreCase).Replace(s, sprintf "$& class=\"%s\"" (newClass + "A"))

        let private stripSteamRedirects (s: string) = Regex(steamLinkFilter, RegexOptions.IgnoreCase).Replace(s, "")

        let rec private trimEndBr (s: string) =
            if s.EndsWith(@"<br>") then trimEndBr (s.Substring(0, s.Length - 4))
            else s

        /// Formats an rss feed by stripping classes and adding new ones
        let formatRawHtml (newClass: string) (sList: (string * string) list) =
            sList
            |> List.map (fun (title, body) ->
                title,
                body.Trim()
                |> trimEndBr
                |> stripClasses
                |> addImgClasses newClass
                |> addAClasses newClass
                |> stripSteamRedirects)

    /// Module to hold all the engine.ini modification operations
    module rec EngineMods =
        let cosmetics =
            [ { Title = "Sharpen picture"
                Caption = "Increase rendering sharpness, particularly useful \
                 when using high amounts of anti-aliasing."
                Settings =
                    [ { Key = @"r.Tonemapper.Sharpen"
                        Default = KeyValues.Values.Float(2.)
                        Value = None
                        Mutable =
                            { KeyValues.Mutable.Min = KeyValues.MutableValues.MutFloat(0.)
                              KeyValues.Mutable.Max = KeyValues.MutableValues.MutFloat(3.)
                              KeyValues.Mutable.Step = 0.25 }
                            |> Some } ]
                File = ConfigFile.Engine
                Enabled = false
                Expanded = false }
              { Title = "Disable sun glare"
                Caption = "Disables or reduces sun glare effects in game. \
                 For best results set shadows to low in the game UI."
                Settings =
                    [ { Key = @"r.LightShaftBlurPasses"
                        Default = KeyValues.Values.Int(0)
                        Value = None
                        Mutable = None }
                      { Key = @"r.BloomQuality"
                        Default = KeyValues.Values.Int(0)
                        Value = None
                        Mutable = None }
                      { Key = @"r.MotionBlurQuality"
                        Default = KeyValues.Values.Int(0)
                        Value = None
                        Mutable = None } ]
                File = ConfigFile.Engine
                Enabled = false
                Expanded = false }
              { Title = "Disable Fisheye Effect"
                Caption = "Enables artificial panini projection, helps if fisheye \
                 from high fov is bothersome."
                Settings =
                    [ { Key = @"r.upscale.panini.d"
                        Default = KeyValues.Values.Float(0.1)
                        Value = None
                        Mutable =
                            { KeyValues.Mutable.Min = KeyValues.MutableValues.MutFloat(0.)
                              KeyValues.Mutable.Max = KeyValues.MutableValues.MutFloat(1.)
                              KeyValues.Mutable.Step = 0.05 }
                            |> Some }
                      { Key = @"r.upscale.panini.s"
                        Default = KeyValues.Values.Float(0.025)
                        Value = None
                        Mutable =
                            { KeyValues.Mutable.Min = KeyValues.MutableValues.MutFloat(0.)
                              KeyValues.Mutable.Max = KeyValues.MutableValues.MutFloat(0.25)
                              KeyValues.Mutable.Step = 0.025 }
                            |> Some } ]
                File = ConfigFile.Engine
                Enabled = false
                Expanded = false }
              { Title = "Disable fog"
                Caption = "Removes additional fog effects from maps."
                Settings =
                    [ { Key = @"r.Fog"
                        Default = KeyValues.Values.Int(0)
                        Value = None
                        Mutable = None } ]
                File = ConfigFile.Engine
                Enabled = false
                Expanded = false } ]

        let utilities =
            [ { Title = "Enable network parry debug"
                Caption = "Enables a utility that will print a small line of red \
                 text when your parry was correct but missed due to latency."
                Settings =
                    [ { Key = @"m.DebugNetworkParry"
                        Default = KeyValues.Values.Int(1)
                        Value = None
                        Mutable = None } ]
                File = ConfigFile.Engine
                Enabled = false
                Expanded = false }
              { Title = "Enable matchmaking debug"
                Caption = "Enables a utility that will matchmaking debug text \
                  allowing you to see raw MMR."
                Settings =
                    [ { Key = @"m.ShowMatchmakingDebug"
                        Default = KeyValues.Values.Int(1)
                        Value = None
                        Mutable = None } ]
                File = ConfigFile.Engine
                Enabled = false
                Expanded = false }
              { Title = "Skip intro cut scenes"
                Caption = "This will disable the intro videos from playing."
                Settings =
                    [ { Key = @"SkipStartupMovies"
                        Default = KeyValues.Values.Int(1)
                        Value = None
                        Mutable = None } ]
                File = ConfigFile.GameUserSettings
                Enabled = false
                Expanded = false } ]

        let performance =
            [ { Title = "Enable Runescape mode"
                Caption = "Significantly improves game performance at the cost of playing \
                 a game that looks like it was not made in the past decade."
                Settings =
                    [ { Key = @"r.mipmaplodbias"
                        Default = KeyValues.Values.Int(500)
                        Value = None
                        Mutable = None }
                      { Key = @"r.skeletalmeshlodbias"
                        Default = KeyValues.Values.Int(500)
                        Value = None
                        Mutable = None } ]
                File = ConfigFile.Engine
                Enabled = false
                Expanded = false } ]

        let quality =
            [ { Title = "Enable texture pre-loading"
                Caption = "Increases system load by roughly 30% and in return \
                 reduces rendering hitches. Only recommended for \
                 top end gaming desktops. This will not provide a benefit \
                 if your graphics card does not have a significant amount \
                 of dedicated memory."
                Settings =
                    [ { Key = @"r.Streaming.Boost"
                        Default = KeyValues.Values.Int(0)
                        Value = None
                        Mutable = None }
                      { Key = @"r.Streaming.PoolSize"
                        Default = KeyValues.Values.Int(0)
                        Value = None
                        Mutable = None }
                      { Key = @"r.Streaming.HLODStrategy"
                        Default = KeyValues.Values.Int(2)
                        Value = None
                        Mutable = None }
                      { Key = @"r.Streaming.FullyLoadUsedTextures"
                        Default = KeyValues.Values.Int(1)
                        Value = None
                        Mutable = None }
                      { Key = @"r.bForceCPUAccessToGPUSkinVerts"
                        Default = KeyValues.Values.Bool(true)
                        Value = None
                        Mutable = None }
                      { Key = @"r.CreateShadersOnLoad"
                        Default = KeyValues.Values.Int(1)
                        Value = None
                        Mutable = None }
                      { Key = @"r.Shaders.Optimize"
                        Default = KeyValues.Values.Int(1)
                        Value = None
                        Mutable = None } ]
                File = ConfigFile.Engine
                Enabled = false
                Expanded = false } ]

    /// Toastr bindings
    module Toastr =
        open Elmish

        importAll "toastr/build/toastr.min.css"

        type ToastrMsg =
            { Message: string
              Title: string }

        let private successToast (msg: string): unit = import "success" "toastr"
        let private successToastWithTitle (msg: string) (title: string): unit = import "success" "toastr"
        let private errorToast (msg: string): unit = import "error" "toastr"
        let private errorToastWithTitle (msg: string) (title: string): unit = import "error" "toastr"
        let private infoToast (msg: string): unit = import "info" "toastr"
        let private infoToastWithTitle (msg: string) (title: string): unit = import "info" "toastr"
        let private warningToast (msg: string): unit = import "warning" "toastr"
        let private warningToastWithTitle (msg: string) (title: string): unit = import "warning" "toastr"

        let message msg =
            { Message = msg
              Title = "" }

        let withTitle title msg: ToastrMsg = { msg with Title = title }

        let success (msg: ToastrMsg): Cmd<_> =
            [ fun _ ->
                if System.String.IsNullOrEmpty(msg.Title) then successToast msg.Message
                else successToastWithTitle msg.Message msg.Title ]

        let error (msg: ToastrMsg): Cmd<_> =
            [ fun _ ->
                if System.String.IsNullOrEmpty(msg.Title) then errorToast msg.Message
                else errorToastWithTitle msg.Message msg.Title ]

        let info (msg: ToastrMsg): Cmd<_> =
            [ fun _ ->
                if System.String.IsNullOrEmpty(msg.Title) then infoToast msg.Message
                else infoToastWithTitle msg.Message msg.Title ]

        let warning (msg: ToastrMsg): Cmd<_> =
            [ fun _ ->
                if System.String.IsNullOrEmpty(msg.Title) then warningToast msg.Message
                else warningToastWithTitle msg.Message msg.Title ]

    /// MaterialUI library extensions
    module MaterialUI =
        open Fable.React
        open Fable.React.Props
        open Fable.MaterialUI
        open Fable.MaterialUI.Core

        module Core =
            let inline slider (b: IHTMLProp seq) c: ReactElement =
                ofImport "default" "@material-ui/core/Slider" (toObj b) c
            let inline skeleton (b: IHTMLProp seq): ReactElement =
                ofImport "default" "@material-ui/lab/Skeleton" (toObj b) []

        [<AutoOpen>]
        module Themes =
            module Styles =
                let inline Marked props = Custom("marked", props)
                let inline Rail props = Custom("rail", props)
                let inline Track props = Custom("track", props)
                let inline Thumb props = Custom("thumb", props)
                let inline ValueLabel props = Custom("valueLabel", props)
                let inline Mark props = Custom("mark", props)
                let inline MarkActive props = Custom("markActive", props)
                let inline MarkLabel props = Custom("markLabel", props)
                let inline MarkLabelActive props = Custom("markLabelActive", props)
                let inline Rect props = Custom("rect", props)
                let inline Animate props = Custom("animate", props)

            type ClassNames =
                | Marked of string
                | Rail of string
                | Track of string
                | Thumb of string
                | ValueLabel of string
                | Mark of string
                | MarkActive of string
                | MarkLabel of string
                | MarkLabelActive of string
                | Rect of string
                | Animate of string
                interface IClassNames

        [<AutoOpen>]
        module Props =
            type SliderPropMarkValue =
                { Value: float
                  Label: string option }
                static member Pojo(marks: SliderPropMarkValue) = marks |> toPlainJsObj

            [<StringEnum; RequireQualifiedAccess>]
            type SliderOrientation =
                | Vertical
                | Horizontal

            [<StringEnum; RequireQualifiedAccess>]
            type SliderLabelDisplay =
                | On
                | Auto
                | Off

            type SliderProp =
                | GetAriaValueText of (obj -> obj -> unit)
                | Marks of U2<bool, obj []>
                | Name of string
                | Min of float
                | Max of float
                | OnChange of (obj -> obj -> unit)
                | OnChangeCommitted of (obj -> obj -> unit)
                | Orientation of SliderOrientation
                | Step of float
                | ThumbComponent of ReactElementType
                | ValueLabelDisplay of SliderLabelDisplay
                | ValueLabelFormat of U2<string, float -> float>
                interface IHTMLProp

            [<StringEnum; RequireQualifiedAccess>]
            type SkeletonVariant =
                | Text
                | Rect
                | Circle

            type SkeletonProp =
                | DisableAnimate of bool
                | Variant of SkeletonVariant
                interface IHTMLProp

            [<AutoOpen>]
            module OverridesProp =
                let inline private pascalCaseProp (name: string) (props: Themes.IStyles seq) =
                    OverridesProp.Custom(name, props |> keyValueList CaseRules.LowerFirst)

                let inline MuiSlider styles = pascalCaseProp "MuiSlider" styles
                let inline MuiSkeleton styles = pascalCaseProp "MuiSkeleton" styles

            [<AutoOpen>]
            module ThemePropsProp =
                let inline private pascalCaseProp (name: string) (props: IHTMLProp seq) =
                    ThemePropsProp.Custom(name, props |> keyValueList CaseRules.LowerFirst)

                let inline MuiSlider props = pascalCaseProp "MuiSlider" props
                let inline MuiSkeleton styles = pascalCaseProp "MuiSkeleton" styles
