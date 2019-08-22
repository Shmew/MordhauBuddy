﻿namespace MordhauBuddy.App

module BridgeUtils =
    open MordhauBuddy.Shared.ElectronBridge

    type INIBridgeSender(caller : Caller) =
        let wrapOps iCmd = BridgeOps(INIOperation(iCmd), caller)
        let wrapFace fCmd = BridgeOps(Faces(fCmd), caller)
        let wrapConf cCmd = BridgeOps(Configs(cCmd), caller)
        member this.Replace s sels iFile = INIFileOperation.Replace(s,sels,iFile) |> wrapOps
        member this.Delete sels = INIFileOperation.Delete(sels) |> wrapOps
        member this.Exists iFile = INIFileOperation.Exists(iFile) |> wrapOps
        member this.Parse iFile = INIFileOperation.Parse(iFile) |> wrapOps
        member this.Backup iList = INIFileOperation.Backup(iList) |> wrapOps
        member this.DefaultDir = INIFileOperation.DefaultDir |> wrapOps
        member this.Commit iList = INIFileOperation.Commit(iList) |> wrapOps
        member this.SetRandom profile = Random(profile) |> wrapFace
        member this.SetFrankenstein profile = Frankenstein(profile) |> wrapFace
        member this.SetCustom profile fVal = Custom(profile,fVal) |> wrapFace
        member this.GetProfileList = ProfileList |> wrapFace
        member this.GetConfigs oList = GetConfigs(oList) |> wrapConf
        member this.MapConfigs oList = MapConfigs(oList) |> wrapConf

    type MapBridgeSender(caller : Caller) =
        let wrapOps mCmd = BridgeOps(MapOperation(mCmd), caller)
        let wrapMaps mCmd = BridgeOps(Maps(mCmd), caller)
        member this.DefaultDir = MapFileOperation.DefaultDir |> wrapOps
        member this.DirExists s = MapFileOperation.DirExists(s) |> wrapOps
        member this.GetAvailable = Maps.GetAvailableMaps |> wrapMaps
        member this.GetInstalled s = Maps.GetInstalledMaps s |> wrapMaps

module RenderUtils =
    open Electron
    open Fable.Core
    open Fable.Core.JsInterop
    open Fable.Import
    open Browser.Types
    open Node.Api
    
    let isWindows = Node.Api.``process``.platform = Node.Base.Platform.Win32
    
    let getRemoteWin() = renderer.remote.getCurrentWindow()

    [<Emit("$0.persist()")>]
    let eventPersist (e : Event) : unit = jsNative
    
    [<Emit("try{document.elementFromPoint($0, $1)}catch(e){}")>]
    let getElementAtPos (x : int) (y : int) : HTMLElement option = jsNative

    let getMousePositions () = 
        let absMouse = renderer.remote.screen.getCursorScreenPoint()
        let absWindow = renderer.remote.getCurrentWindow().getBounds()
        {| X = absMouse.x - absWindow.x; Y = absMouse.y - absWindow.y |}

    /// Prefixes the string with the static asset root path.
    let stat (s : string) =
#if DEBUG
        s
#else
        path.resolve (__dirname, "..", "..", "static", s)
#endif

    module MapTypes =
        open System

        [<RequireQualifiedAccess>]
        type MapVersion =
            { Major : int
              Minor : int
              Patch : int }
            static member Create(major, minor, patch) =
                { MapVersion.Major = major
                  MapVersion.Minor = minor
                  MapVersion.Patch = patch }
            member this.GetString() =
                sprintf "%i.%i.%i" this.Major this.Minor this.Patch

        [<Measure>] type KB
        [<Measure>] type MB
        [<Measure>] type GB
        
        let kbPerMb : float<KB/MB> = 1000.<KB/MB>
        let gbPerMb : float<GB/MB> = 0.001<GB/MB>

        let convertKBtoMB (x : float<KB>) = x / kbPerMb
        let convertGBtoMB (x : float<GB>) = x / gbPerMb

        [<RequireQualifiedAccess>]
        type PlayerRange =
            { Min : int
              Max : int }

        [<RequireQualifiedAccess>]
        type SuggestedPlayers =
            | Range of PlayerRange
            | Static of int

        type CommunityMap =
            { Name : string option
              Folder : string
              Description : string option
              Author : string option
              Version : MapVersion
              ReleaseDate : DateTime option
              FileSize : float<MB> option
              Players : SuggestedPlayers option
              Image : string option }
            member this.GetName() =
                match this.Name with
                | Some(name) -> name
                | None -> this.Folder

            member this.GetPlayers() =
                match this.Players with
                | Some(p) ->
                    match p with
                    | SuggestedPlayers.Range range -> 
                        sprintf "%i - %i" range.Min range.Max 
                        |> Some
                    | SuggestedPlayers.Static i -> 
                        sprintf "%i" i |> Some
                | None -> None

            member this.GetDate() =
                this.ReleaseDate 
                |> Option.map (fun d -> d.ToString("dd-MM-yyyy"))

            member this.GetMetaData() =
                let desc = this.Description |> Option.map (fun s -> sprintf "%s%c" s '\n')
                let author =
                    this.Author |> Option.map (fun s -> sprintf "Author: %s" s)
                let date =
                    this.ReleaseDate 
                    |> Option.bind (fun d -> 
                        if d.ToString() = "Invalid Date" then None 
                        else (sprintf "Release Date: %s" <| d.ToString("dd-MM-yyyy")) |> Some)
                let fileSize =
                    this.FileSize 
                    |> Option.map (fun s -> sprintf "File size: %s MB" <| s.ToString())
                let players = 
                    this.GetPlayers() 
                    |> Option.map (fun s -> sprintf "Players: %s" s)
                    
                [ desc; author; date; fileSize; players ]
                |> List.choose id

    module String =
        open System.Text.RegularExpressions

        let ensureEndsWith (suffix : string) (str : string) =
            if str.EndsWith suffix then str
            else str + suffix

        let duToTitle (s : string) =
            MatchEvaluator(fun m -> " " + m.Value)
            |> (fun m -> Regex.Replace(s.Substring(1), "[A-Z]", m))
            |> (+) (s.Substring(0, 1))

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

        let errorStrings (res: Result<string,string list>) =
            match res with
            | Ok (s : string) -> s
            | Error (err : string list) -> 
                err |> List.reduce (fun acc elem -> acc + " " + elem)

    module Validation =
        open System.Text.RegularExpressions

        module RegPatterns =
            let translate = @"^\(Translate=\((\d*,?){49}\),Rotate=\((\d*,?){49}\),Scale=\((\d*,?){49}\)\)"
            let fileSize = @"^(\d+[, .]\d+|\d+)\s*(kb|mb|gb)$"
            let semVersion = @"^(\d+)[. ,]*(\d+)*[. ,]*(\d)*"
            let players = @"^(\d+).*?(\d+)$"

        [<AutoOpen>]
        module internal Helpers =
            open System
            open MapTypes

            let removeAllWs (s : string) =
                s.Trim().ToCharArray() 
                |> Array.choose (fun c -> 
                    if c = ' ' then None else Some(string c)) 
                |> Array.reduce (+)

            let applyRPattern (pattern : string) (input : string) f = 
                try
                    Regex(pattern, RegexOptions.IgnoreCase).Match(input)
                    |> fun m -> seq { for items in m.Groups do yield items }
                    |> List.ofSeq 
                    |> List.tail 
                    |> List.map (fun g -> g.Value |> f)
                with
                | _ -> []

            let getFileSize (s : string) =
                let matchSize =
                    applyRPattern RegPatterns.fileSize (removeAllWs s) id

                let convUnit (s : string) (f : float) =
                    match s.ToLower() with
                    | "kb" -> 1.0<KB> * f |> convertKBtoMB |> Some
                    | "mb" -> 1.0<MB> * f |> Some
                    | "gb" -> 1.0<GB> * f |> convertGBtoMB |> Some
                    | _ -> None

                match matchSize with
                | [num; sizeUnit] -> 
                    match Double.TryParse num with
                    | (true, f) -> Some(f)
                    | _ -> None
                    |> Option.bind (convUnit sizeUnit)
                | _ -> None

            let private getInt (s : string) =
                match Int32.TryParse s with
                | (true, i) -> i
                | _ -> 0

            let getVer (vStr : string) =
                applyRPattern RegPatterns.semVersion vStr getInt
                |> fun iList ->
                match iList.Length with
                | i when i >= 3 -> iList |> List.take 3
                | i -> List.init (3 - i) (fun _ -> 0) |> List.append iList
                |> fun iL -> MapVersion.Create(iL.[0], iL.[1], iL.[2])

            let getDate (s : string) =
                match DateTime.TryParse(s) with
                | (true, dt) -> dt |> Some
                | _ -> None

            let getPlayers (s : string) =
                match Int32.TryParse s with
                | (true, i) ->
                    SuggestedPlayers.Static(i)
                    |> Some
                | _ ->
                    applyRPattern RegPatterns.players s getInt
                    |> fun pList ->
                    match pList with
                    | [min;max] when pList.Length = 2 -> { PlayerRange.Min = min; PlayerRange.Max = max } |> SuggestedPlayers.Range |> Some
                    | _ -> None

        let validateDir (s : string) = Fable.Validation.Core.single <| fun t ->
            t.TestOne s
                |> t.IsValid (fun _ -> 
                    try 
                        s |> path.normalize |> path.parse |> ignore
                        true
                    with 
                    | _ -> false) "Invalid path"
                |> t.Trim
                |> t.NotBlank "Directory cannot be blank"
                |> t.End

        let validateImport (s : string) = Fable.Validation.Core.single <| fun t ->
            t.TestOne s
                |> t.Trim
                |> t.NotBlank "Must provide import string"
                |> t.Match (Regex RegPatterns.translate) "Invalid import string"
                |> t.End

        let validateUri (s : string) = Fable.Validation.Core.single <| fun t ->
            t.TestOne s
                |> t.Trim
                |> t.NotBlank ""
                |> t.IsUrl s
                |> t.End

        let validateVer (s : string) = Fable.Validation.Core.single <| fun t ->
            t.TestOne s
                |> t.Trim
                |> t.Map getVer
                |> t.End

        let validateFileSize (s : string) = Fable.Validation.Core.single <| fun t ->
            t.TestOne s
                |> t.Trim
                |> t.NotBlank ""
                |> t.Map getFileSize
                |> t.IsSome ""
                |> t.End

        let validateDate (s : string) = Fable.Validation.Core.single <| fun t ->
            t.TestOne s
                |> t.Trim
                |> t.NotBlank ""
                |> t.Map getDate
                |> t.IsSome ""
                |> t.End

        let validatePlayers (s : string) = Fable.Validation.Core.single <| fun t ->
            t.TestOne s
                |> t.Trim
                |> t.NotBlank ""
                |> t.Map getPlayers
                |> t.IsSome ""
                |> t.End

        let validateGeneric (s : string) = Fable.Validation.Core.single <| fun t ->
            t.TestOne s
                |> t.Trim
                |> t.NotBlank ""
                |> t.End

    module Directory =
        open MordhauBuddy.Shared.ElectronBridge

        type Dirs =
            { Game : string
              Engine : string
              GameUser : string
              Maps : string }

        type DirLoad =
            | ConfigFiles of ConfigFile
            | MapDir

        type ConfigDir =
            { Dir : DirLoad
              Waiting : bool  
              Directory : string
              Error : bool
              Label : string
              HelperText : string
              Validated : bool }

        [<RequireQualifiedAccess>]
        type DirSelect =
            | Selected of string
            | Canceled

        let selectDir () =
            promise {
                let opts = jsOptions<OpenDialogOptions>(fun o ->
                    /// See https://github.com/electron/electron/blob/master/docs/api/dialog.md
                    o.title <- "Select Mordhau Configuration Directory"
                    o.defaultPath <- renderer.remote.app.getPath AppPathName.Home
                    o.properties <- [| DialogFeature.OpenDirectory |]
                )
                let! res = renderer.remote.dialog.showOpenDialog opts
                if res.canceled then return DirSelect.Canceled
                else
                    return res.filePaths |> Array.head |> DirSelect.Selected
            }

    module Samples =
        let faceImport =
            "(Translate=(65535,31072,875,704,0,734,65535,0,0,65535,31565,0,0,65535,0,29632,29662,30686,\
            65535,65535,0,30720,65535,0,0,918,31560,0,65535,31709,31680,544,574,30749,30720,256,286,65535,\
            0,0,0,65535,0,0,65535,0,65535,31678,31648),Rotate=(0,65535,26302,31680,0,30750,0,65535,0,0,0,\
            65535,65535,65535,31584,30749,31648,8,65535,0,65535,608,65535,65535,0,31695,893,18301,65535,31677,\
            30720,31704,30725,1,988,29,960,0,65535,0,65535,65535,16326,0,65535,65535,15383,30,960),Scale=(0,30,\
            4139,30749,65535,30749,0,65535,65535,0,0,0,0,65535,31709,0,0,190,0,0,0,589,0,0,0,30749,31166,989,\
            65535,5085,5085,4242,4242,0,0,24452,24452,65535,0,0,65535,65535,574,0,0,65535,574,21470,21470))"

    module WebParsing =
        open System
        open Validation
        open MapTypes

        /// Because Json is too complex
        let getComMap (sInfoFile : string) =
            let infoArr = sInfoFile.Split([| "\r\n"; "\r"; "\n" |], StringSplitOptions.None)

            let mapResult res =
                match res with
                | Ok(s) -> Some(s)
                | _ -> None

            let validateInfo (i : int) (vFun : string -> Result<'t,string list>) =
                infoArr.[i]
                |> vFun
                |> mapResult

            let vGeneric (i : int) = validateInfo i validateGeneric

            { Name = vGeneric 0
              Folder = infoArr.[1].Trim()
              Description = vGeneric 2
              Author = vGeneric 3
              Version = 
                validateInfo 4 validateVer 
                |> Option.orElse (MapTypes.MapVersion.Create(0,0,0) |> Some) 
                |> Option.get
              ReleaseDate = validateInfo 5 validateDate
              FileSize = validateInfo 6 validateFileSize
              Players = validateInfo 7 validatePlayers
              Image = validateInfo 8 validateUri }

    module rec EngineMods =
        open MordhauBuddy.Shared.ElectronBridge

        let cosmetics = [
            { Title = "Sharpen picture"
              Caption = 
                "Increase rendering sharpness, particularly useful \
                 when using high amounts of anti-aliasing."
              Settings = [
                  { Key = @"r.Tonemapper.Sharpen"
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
              Caption = 
                "Disables or reduces sun glare effects in game. \
                 For best results set shadows to low in the game UI."
              Settings = [
                  { Key = @"r.LightShaftBlurPasses"
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
              Caption = 
                "Enables artificial panini projection, helps if fisheye \
                 from high fov is bothersome."
              Settings = [
                  { Key = @"r.upscale.panini.d"
                    Default = KeyValues.Values.Float(0.1)
                    Value = None 
                    Mutable =
                        { KeyValues.Mutable.Min = KeyValues.MutableValues.MutFloat(0.)
                          KeyValues.Mutable.Max = KeyValues.MutableValues.MutFloat(1.)
                          KeyValues.Mutable.Step = 0.1 }
                        |> Some }
                  { Key = @"r.upscale.panini.s"
                    Default = KeyValues.Values.Float(0.025)
                    Value = None
                    Mutable =
                        { KeyValues.Mutable.Min = KeyValues.MutableValues.MutFloat(0.)
                          KeyValues.Mutable.Max = KeyValues.MutableValues.MutFloat(0.25) 
                          KeyValues.Mutable.Step = 0.025 }
                        |> Some }]
              File = ConfigFile.Engine 
              Enabled = false
              Expanded = false }
            { Title = "Disable fog"
              Caption = "Removes additional fog effects from maps."
              Settings = [
                  { Key = @"r.Fog"
                    Default = KeyValues.Values.Int(0)
                    Value = None
                    Mutable = None } ]
              File = ConfigFile.Engine 
              Enabled = false
              Expanded = false } ]

        let utilities = [
            { Title = "Enable network parry debug"
              Caption = 
                "Enables a utility that will print a small line of red \
                 text when your parry was correct but missed due to latency."
              Settings = [
                  { Key = @"m.DebugNetworkParry"
                    Default = KeyValues.Values.Int(1)
                    Value = None
                    Mutable = None } ]
              File = ConfigFile.Engine 
              Enabled = false
              Expanded = false  }
            { Title = "Skip intro cut scenes"
              Caption = 
                "This will disable the intro videos from playing."
              Settings = [
                  { Key = @"SkipStartupMovies"
                    Default = KeyValues.Values.Int(1)
                    Value = None
                    Mutable = None } ]
              File = ConfigFile.GameUserSettings 
              Enabled = false
              Expanded = false  } ]

        let performance = [
            { Title = "Enable Runescape mode"
              Caption = 
                "Significantly improves game performance at the cost of playing \
                 a game that looks like it was not made in the past decade."
              Settings = [
                  { Key = @"r.mipmaplodbias"
                    Default = KeyValues.Values.Int(500)
                    Value = None 
                    Mutable = None } 
                  { Key = @"r.skeletalmeshlodbias"
                    Default = KeyValues.Values.Int(500)
                    Value = None
                    Mutable = None } ]
              File = ConfigFile.Engine 
              Enabled = false
              Expanded = false  } ]

        let quality = [
            { Title = "Enable texture pre-loading"
              Caption = 
                "Increases system load by roughly 30% and in return \
                 reduces rendering hitches. Only recommended for \
                 top end gaming desktops. This will not provide a benefit \
                 if your graphics card does not have a significant amount \
                 of dedicated memory."
              Settings = [
                  { Key = @"r.Streaming.Boost"
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
              Expanded = false  } ]

    module Toastr =
        open Elmish

        importAll "toastr/build/toastr.min.css"

        type ToastrMsg =
            { Message : string
              Title : string }

        let private successToast (msg : string) : unit = import "success" "toastr"
        let private successToastWithTitle (msg : string) (title : string) : unit = import "success" "toastr"
        let private errorToast (msg : string) : unit = import "error" "toastr"
        let private errorToastWithTitle (msg : string) (title : string) : unit = import "error" "toastr"
        let private infoToast (msg : string) : unit = import "info" "toastr"
        let private infoToastWithTitle (msg : string) (title : string) : unit = import "info" "toastr"
        let private warningToast (msg : string) : unit = import "warning" "toastr"
        let private warningToastWithTitle (msg : string) (title : string) : unit = import "warning" "toastr"

        let message msg =
            { Message = msg
              Title = "" }

        let withTitle title msg = { msg with Title = title }

        let success (msg : ToastrMsg) : Cmd<_> =
            [ fun _ -> 
                if System.String.IsNullOrEmpty(msg.Title) then successToast msg.Message
                else successToastWithTitle msg.Message msg.Title ]

        let error (msg : ToastrMsg) : Cmd<_> =
            [ fun _ -> 
                if System.String.IsNullOrEmpty(msg.Title) then errorToast msg.Message
                else errorToastWithTitle msg.Message msg.Title ]

        let info (msg : ToastrMsg) : Cmd<_> =
            [ fun _ -> 
                if System.String.IsNullOrEmpty(msg.Title) then infoToast msg.Message
                else infoToastWithTitle msg.Message msg.Title ]

        let warning (msg : ToastrMsg) : Cmd<_> =
            [ fun _ -> 
                if System.String.IsNullOrEmpty(msg.Title) then warningToast msg.Message
                else warningToastWithTitle msg.Message msg.Title ]

    module MaterialUI =
        open Fable.React
        open Fable.React.Props
        open Fable.MaterialUI
        open Fable.MaterialUI.Core

        module Core =
            let inline slider (b : IHTMLProp seq) c : ReactElement = 
                ofImport "default" "@material-ui/core/Slider" (toObj b) c
            let inline skeleton (b : IHTMLProp seq) : ReactElement =
                ofImport "default" "@material-ui/lab/Skeleton" (toObj b) []

        [<AutoOpen>]
        module Themes =
            module Styles =
                let inline Marked props = Custom ("marked", props)
                let inline Rail props = Custom ("rail", props)
                let inline Track props = Custom ("track", props)
                let inline Thumb props = Custom ("thumb", props)
                let inline ValueLabel props = Custom ("valueLabel", props)
                let inline Mark props = Custom ("mark", props)
                let inline MarkActive props = Custom ("markActive", props)
                let inline MarkLabel props = Custom ("markLabel", props)
                let inline MarkLabelActive props = Custom ("markLabelActive", props)
                let inline Rect props = Custom ("rect", props)
                let inline Animate props = Custom ("animate", props)
            
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
                { Value : float
                  Label : string option }
                static member Pojo (marks: SliderPropMarkValue) =
                    marks |> toPlainJsObj

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
                | ValueLabelFormat of U2<string,(float -> float)>
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
                let inline private pascalCaseProp (name : string) (props : Themes.IStyles seq) =
                    OverridesProp.Custom (name, props |> keyValueList CaseRules.LowerFirst)

                let inline MuiSlider styles = pascalCaseProp "MuiSlider" styles
                let inline MuiSkeleton styles = pascalCaseProp "MuiSkeleton" styles

            [<AutoOpen>]
            module ThemePropsProp =
                let inline private pascalCaseProp (name : string) (props : IHTMLProp seq) =
                    ThemePropsProp.Custom (name, props |> keyValueList CaseRules.LowerFirst)

                let inline MuiSlider props = pascalCaseProp "MuiSlider" props
                let inline MuiSkeleton styles = pascalCaseProp "MuiSkeleton" styles
