namespace MordhauBuddy.Core

open Fake.Core
open Fake.IO
open Fake.IO.FileSystemOperators
open INIReader
open System

/// Module for doing file operations
module FileOps =
    /// File operations for INI files
    module INI =
        open MordhauBuddy.Shared.ElectronBridge

        /// Try to find the configuration directory
        let defaultDir =
            let bindDirectory (dir: string) =
                IO.DirectoryInfo dir
                |> DirectoryInfo.exists
                |> function
                | true -> Some(dir)
                | false -> None
            match Environment.isWindows with
            | true ->
                Environment.SpecialFolder.LocalApplicationData
                |> Environment.GetFolderPath
                |> (fun fol -> fol @@ @"Mordhau\Saved\Config\WindowsClient")
                |> bindDirectory
            | false ->
                [ ".steam/steam"; ".local/share/Steam" ]
                |> List.map (fun s -> (Environment.SpecialFolder.UserProfile |> Environment.GetFolderPath) @@ s)
                |> List.tryFind (bindDirectory >> Option.isSome)
                |> Option.bind
                    (fun dir ->
                    dir
                    @@ "steamapps/compatdata/629760/pfx/drive_c/users/steamuser/AppData/Local"
                       @@ "Mordhau/Saved/Config/WindowsClient" |> Some)
                |> Option.bind bindDirectory

        /// Try to find the file given an `INIFile`
        let tryGetFile (file: string) (workingDir: string option) =
            let fiPath = IO.FileInfo(file).FullName
            match workingDir, (fiPath = file && File.exists file) with
            | _, true -> Some(file)
            | Some(dir), _ when File.exists (dir @@ file) -> Some(dir @@ file)
            | _ -> None

        /// Create a backup of the given file into sub directory MordhauBuddy_backups
        let createBackup (file: string) =
            let fi = FileInfo.ofPath (file)
            match File.exists file with
            | true ->
                let backups = fi.DirectoryName @@ "MordhauBuddy_backups"
                let newName = (fi.Name.Split('.').[0]) + DateTime.Now.ToString("yyyyMMdd-hhmm") + ".ini"
                Directory.ensure backups
                Shell.copyFile (backups @@ newName) file
                File.exists (backups @@ newName)
            | false -> false

        /// Clean backups directory based on given backup settings
        let cleanBackups (bSet: BackupSettings) (file: string) =
            try
                match bSet with
                | KeepAll -> ()
                | KeepLast10 ->
                    FileInfo.ofPath(file).Directory.GetDirectories("*MordhauBuddy_backups*")
                    |> Array.iter (fun dir ->
                        dir.GetFiles()
                        |> Array.groupBy (fun f ->
                            match f with
                            | f when f.Name.StartsWith("GameUserSettings") -> "GameUserSettings"
                            | f when f.Name.StartsWith("Engine") -> "Engine"
                            | f when f.Name.StartsWith("Game") -> "Game"
                            | _ -> "")
                        |> Array.iter (fun (k, fArr) ->
                            if k = "" then ()
                            else
                                fArr
                                |> Array.sortBy (fun f -> f.CreationTime)
                                |> Array.rev
                                |> Array.indexed
                                |> Array.iter (fun (i, f) ->
                                    if i > 9 then Shell.rm f.FullName)))
                | NoBackups ->
                    FileInfo.ofPath(file).Directory.GetDirectories("*MordhauBuddy_backups*")
                    |> Array.map (fun d -> d.FullName)
                    |> Shell.cleanDirs
            with e ->
#if DEBUG
                failwith e.Message
#endif
                ()

        /// Write `INIValue` to file path
        let writeINI (iVal: INIValue) (outFile: string) =
            let fi = FileInfo.ofPath (outFile)
            Directory.ensure fi.DirectoryName
            File.writeString false fi.FullName (iVal.ToString())
            tryGetFile outFile None

        /// Try to read an INI file
        let tryReadINI (file: string) =
            if File.exists file then
                try
                    File.readAsString file |> INIValue.TryParse
                with _ -> None
            else None

    /// File operations for maps
    module Maps =
        open Fake.IO.Globbing.Operators

        /// Try to find the Map directory
        let defaultDir =
            let mapPath =
                match Environment.isWindows with
                | true ->
                    [ @"C:\Program Files (x86)"; @"C:\Program Files" ]
                    |> List.map (fun fol -> fol @@ @"Steam\steamapps\common\Mordhau\Mordhau\Content\Mordhau\Maps")
                | false ->
                    [ ".steam/steam"; ".local/share/Steam" ]
                    |> List.map
                        ((fun s -> (Environment.SpecialFolder.UserProfile |> Environment.GetFolderPath) @@ s)
                         >> (fun fol -> fol @@ @"steamapps/common/Mordhau/Mordhau/Content/Mordhau/Maps"))

            let bindDirectory (dir: string) =
                IO.DirectoryInfo dir
                |> DirectoryInfo.exists
                |> function
                | true -> Some(dir)
                | false -> None

            mapPath |> List.tryFind (bindDirectory >> Option.isSome)

        /// Determine if maps directory is valid
        let tryFindMaps (dir: string) =
            let di = IO.DirectoryInfo(dir)
            di.Parent.Name = "Mordhau" && di.Exists && di.FullName.ToLower().Contains("steam")

        /// Get all installed map metadata
        let getInstalled (dir: string) =
            !!(dir @@ "**/*.info.txt")
            |> Seq.map (fun f ->
                async {
                    try
                        return File.readAsString f |> Some
                    with _ -> return None
                })
            |> Async.Parallel
            |> Async.RunSynchronously
            |> List.ofSeq
            |> List.choose id

        /// Try to get the Google Drive API key from embedded resource
        let tryGetGDKey() =
            try
                use stream =
                    new IO.StreamReader(System.Reflection.Assembly.GetExecutingAssembly()
                                              .GetManifestResourceStream("Core.Key.json"))
                stream.ReadToEnd() |> Some
            with _ -> None

        /// Try to delete the given map
        let tryUninstall (dir: string) (fName: string) =
            try
                let folder = new IO.DirectoryInfo(dir @@ fName)
                if DirectoryInfo.exists (folder) then Shell.deleteDir (folder.FullName)
                Ok(true)
            with e -> Error(e.Message)

        /// Delete a zip if it exists
        let asyncDeleteZip (path: string) =
            async {
                if File.exists (path) then Shell.rm path
            }

        /// Delete a directory if it exists
        let deleteDir (path: string) = Shell.deleteDir path

        /// Write an info
        let writeFile (dir: string) (fName: string) (data: string) =
            File.writeString false (dir @@ fName @@ (fName + ".info.txt")) data

    /// File operations for enabling auto launching
    module AutoLaunch =
        open Fake.Windows
        open FSharp.Json
        open Helpers.Info
        open Helpers.Json

        /// Linux DesktopFile
        type DesktopFile =
            { Name: string
              Comment: string
              Version: string
              Type: string
              Terminal: bool
              Exec: string
              Icon: string }
            /// Create the desktop file string
            member this.ToDesktopString() =
                let nL = Environment.NewLine
                let reduceNL sList = sList |> List.reduce (fun acc elem -> acc + nL + elem)
                [ "Name=" + this.Name
                  "Comment=" + this.Comment
                  "Version=" + this.Version
                  "Type=" + this.Type
                  "Terminal=" + (this.Terminal |> string)
                  "Exec=" + this.Exec
                  "Icon=" + this.Icon ]
                |> reduceNL
                |> fun dFile -> "[Desktop Entry]" + nL + dFile

        /// Try to get an env variable with increasing scope
        let private tryGetEnvVar (s: string) =
            let envOpt (envVar: string) =
                if String.isNullOrEmpty envVar then None
                else Some(envVar)

            let procVar = Environment.GetEnvironmentVariable(s) |> envOpt
            let userVar = Environment.GetEnvironmentVariable(s, EnvironmentVariableTarget.User) |> envOpt
            let machVar = Environment.GetEnvironmentVariable(s, EnvironmentVariableTarget.Machine) |> envOpt

            match procVar, userVar, machVar with
            | Some(v), _, _
            | _, Some(v), _
            | _, _, Some(v) -> Some(v)
            | _ -> None

        let private withTimeout timeout action =
            async { let! child = Async.StartChild(action, timeout)
                    return! child }

        /// Try to locate the current executable directory
        let private appPath = Environment.CurrentDirectory

        [<RequireQualifiedAccess>]
        module Windows =

            [<AutoOpen>]
            module Utils =
                let regBase = Registry.RegistryBaseKey.HKEYCurrentUser
                let regKey = @"Software\Microsoft\Windows\CurrentVersion\Run"
                let regSubValue = "MordhauBuddy"

                /// Get Windows executable directory
                let getWinExecDir() =
                    Environment.SpecialFolder.LocalApplicationData
                    |> Environment.GetFolderPath
                    |> fun d -> (new IO.DirectoryInfo(d)).FullName @@ "mordhau-buddy-updater"

                /// Get Windows executable path
                let getWinExecPath() = getWinExecDir() @@ "installer.exe"

            /// Try to enable auto launch
            let enableAutoLaunch() =
                async {
                    return try
                               use registryKey = Registry.getRegistryKey regBase regKey true
                               registryKey.SetValue
                                   (regSubValue, getWinExecPath(), Microsoft.Win32.RegistryValueKind.String)
                               registryKey.Flush()
                               registryKey.Dispose()
                               Async.Sleep 1000 |> Async.RunSynchronously
                               Registry.valueExistsForKey regBase regKey regSubValue
                           with _ -> false
                }
                |> Async.RunSynchronously

            /// Try to disable auto launch
            let disableAutoLaunch() =
                async {
                    return try
                               Registry.deleteRegistryValue regBase regKey regSubValue
                               Registry.valueExistsForKey regBase regKey regSubValue |> not
                           with _ -> false
                }
                |> Async.RunSynchronously

        [<RequireQualifiedAccess>]
        module Linux =
            [<AutoOpen>]
            module Utils =
                /// Try to get home path
                let homePath = tryGetEnvVar "HOME"

                /// Get the name of the AppImage
                let appImageName = sprintf "MordhauBuddy-%s.AppImage" version

                /// Gets the home share falling back to `appPath` if necessary
                let homeShare = defaultArg (homePath |> Option.map (fun p -> p @@ ".local/share/mordhau-buddy")) appPath

                let homeName = sprintf "home-%s.json" version

                /// Set the new MBHome path
                let setMBHome (newFile: string) =
                    { Path = newFile }
                    |> Json.serializeEx config
                    |> File.writeString false (homeShare @@ homeName)

                /// Gets or sets the MordhauBuddy home json if necessary
                let getMBHome() =
                    match FileInfo.ofPath (homeShare @@ homeName) with
                    | hShare when hShare.Exists ->
                        File.readAsString hShare.FullName
                        |> Json.deserializeEx<HomeFile> config
                        |> fun hf -> hf.Path
                    | _ ->
                        let ePath = appPath @@ appImageName
                        { Path = ePath }
                        |> Json.serializeEx config
                        |> File.writeString false (homeShare @@ homeName)
                        ePath

                /// Create desktop file string
                let desktopFile() =
                    { Name = "MordhauBuddy"
                      Comment = "Compilation of Mordhau Tools"
                      Version = version
                      Type = "Application"
                      Terminal = false
                      Exec = getMBHome()
                      Icon =
                          (defaultArg (homePath |> Option.map (fun p -> p @@ ".local/share/mordhau-buddy")) appPath
                           @@ "icon.png") }
                    |> fun dFile -> dFile.ToDesktopString()

                /// Extracts the icon file from the AppImage and stores it in the linux .local folder
                let extractIcon (path: string) =
                    Shell.AsyncExec(appImageName, args = " --appimage-extract static/icon.png", dir = appPath)
                    |> withTimeout 5000
                    |> Async.Ignore
                    |> Async.RunSynchronously
                    try
                        try
                            async
                                { Shell.moveFile (path @@ "mordhau-buddy") (appPath @@ "squashfs-root/static/icon.png") }
                            |> withTimeout 5000
                            |> Async.RunSynchronously
                        with _ -> ()
                    finally
                        async { Shell.deleteDir (appPath @@ "squashfs-root") }
                        |> withTimeout 5000
                        |> Async.RunSynchronously

                /// Create the autostart desktop file
                let createAutoStartDeskFile dir =
                    async {
                        try
                            let autoStart = (dir @@ "autostart")
                            let desktopFilePath = autoStart @@ "mordhau-buddy.desktop"
                            Directory.ensure autoStart
                            do! Async.Sleep 1000
                            File.writeString false desktopFilePath <| desktopFile()
                            do! Async.Sleep 1000
                            return File.exists desktopFilePath
                        with _ -> return false
                    }
                    |> Async.RunSynchronously

            /// Registers the application on linux
            let registerLinuxApp() =
                async {
                    try
                        if Environment.isLinux then
                            homePath
                            |> Option.map (fun path -> path @@ ".local/share")
                            |> function
                            | Some(path) ->
                                Directory.ensure (path @@ "mordhau-buddy")
                                File.writeString false (path @@ "applications/mordhau-buddy.desktop") <| desktopFile()
                                if File.exists (path @@ "mordhau-buddy" @@ "icon.png") |> not then extractIcon path
                            | None -> ()
                    with _ -> ()
                }
                |> Async.Start

            /// Try to enable auto launch
            let enableAutoLaunch() =
                async {
                    return try
                               homePath
                               |> Option.map (fun path -> createAutoStartDeskFile (path @@ ".config"))
                               |> Option.isSome
                           with _ -> false
                }
                |> Async.RunSynchronously

            /// Try to disable auto launch
            let disableAutoLaunch() =
                async {
                    return try
                               homePath
                               |> Option.map (fun path -> path @@ ".config/autostart/mordhau-buddy.desktop")
                               |> function
                               | Some(path) ->
                                   File.delete path
                                   File.exists appPath
                               | None -> false
                               |> not
                           with _ -> false
                }
                |> Async.RunSynchronously

        /// Enable auto launch by calling OS respective function
        let enableAutoLaunch() =
            if Environment.isLinux then Linux.enableAutoLaunch()
            else Windows.enableAutoLaunch()

        /// Disable auto launch by calling OS respective function
        let disableAutoLaunch() =
            if Environment.isLinux then Linux.disableAutoLaunch()
            else Windows.disableAutoLaunch()

        /// Perform additional Linux installation steps
        let registerLinuxApp() =
            if Environment.isLinux then Linux.registerLinuxApp()
            else ()

    /// Fetch and patch new updates
    module Updating =
        open AutoLaunch
        open Fake.Net.Http
        open FastRsync.Delta
        open FastRsync.Diagnostics
        open Helpers.Info
        open Helpers.Http
        open System.IO

        type NewUpdate =
            { LatestRel: Github.Release
              Asset: Github.Asset }

        type UpdatesBehind =
            | Zero
            | One of NewUpdate
            | Multiple of NewUpdate

        /// Try to fetch a list of assets if there is a new release
        let tryGetAssets (releases: Github.Release list) =
            let sorted =
                releases
                |> List.sortByDescending (fun r -> r.TagName.Substring(1) |> Version)
                |> List.indexed

            let updatesBehind =
                sorted
                |> List.tryFind (fun (_, a) -> Version(a.TagName.Substring(1)) = Version(version))
                |> Option.map (fun (i, _) -> i)

            let getRelAt i =
                sorted
                |> List.tryItem (i-1)
                |> Option.map (fun (i, r) -> r, r.TagName.Substring(1))

            let getAssetOf (endsWith: string option) (rel: (Github.Release * string) option) =
                let getEndsWith (name: string) =
                    match endsWith with
                    | Some(e) -> name.EndsWith(e)
                    | None -> true

                match rel with
                | Some(r, ver) ->
                    r.Assets
                    |> List.filter (fun a -> a.Name.StartsWith(appFile ver) && getEndsWith a.Name)
                    |> List.tryHead
                    |> Option.map (fun a ->
                        { LatestRel = r
                          Asset = a })
                | _ -> None

            match updatesBehind with
            | Some 1 ->
                getRelAt 1
                |> getAssetOf (Some ".delta")
                |> function
                | Some res -> Ok <| One res
                | _ -> Error "No available updates"
            | Some 0 -> Ok Zero
            | Some i when i > 1 ->
                getRelAt 0
                |> getAssetOf None
                |> function
                | Some res -> Ok <| Multiple res
                | _ -> Error "Multiple updates behind, asset not found"
            | _ -> Error "Unable to determine update position"

        /// Get base update path
        let private getBaseUpdatePath() =
            if Environment.isLinux then Linux.Utils.homeShare
            else Windows.Utils.getWinExecDir()
            |> fun s -> s @@ "Updating"

        /// Get update path and ensure it exists
        let private getUpdatePath (tagName: string) = getBaseUpdatePath() @@ tagName

        /// Download an assets list
        let private downloadAsset (asset: Github.Asset) (path: IO.DirectoryInfo) =
            try
                downloadFile (path.FullName @@ asset.Name) asset.BrowserDownloadUrl |> Ok
            with e -> Error(e.Message)

        /// Downloads asset file to storage directory
        let getAsset (newUp: NewUpdate) =
            let updatePath =
                let p = getUpdatePath (newUp.LatestRel.TagName)
                Directory.ensure p
                DirectoryInfo.ofPath p

            try
                updatePath
                |> downloadAsset newUp.Asset
                |> Result.bind (fun f ->
                    match FileInfo.ofPath (f) with
                    | fi when fi.Exists -> Ok(fi)
                    | _ -> Error("Error during preparation"))
            with e ->
                try
                    Shell.cleanDir (updatePath.Parent.FullName)
                with _ -> ()
                Error e.Message

        /// Get the file that will be patched
        let getOriginal() =
            if Environment.isLinux then Linux.Utils.getMBHome()
            else Windows.Utils.getWinExecPath()
            |> FileInfo.ofPath

        /// See if file is already downloaded and patched
        let isReady (tagName: string) =
            let newName = appFile (tagName.Substring(1))

            getUpdatePath (tagName) @@ newName
            |> FileInfo.ofPath
            |> fun fi ->
                if fi.Exists then Some(fi.FullName)
                else None

        /// Try to generate a new patch file
        let tryGeneratePatch (deltaFi: FileInfo) =
            let newFile = deltaFi.Directory.FullName @@ (deltaFi.Name.Substring(0, (deltaFi.Name.Length) - 6))

            try
                let delta =
                    let d = new DeltaApplier()
                    d.SkipHashCheck <- true
                    d

                let progressReporter = new ConsoleProgressReporter()

                use basisStream = new FileStream(getOriginal().FullName, FileMode.Open, FileAccess.Read, FileShare.Read)
                use deltaStream = new FileStream(deltaFi.FullName, FileMode.Open, FileAccess.Read, FileShare.Read)
                use newFileStream = new FileStream(newFile, FileMode.Create, FileAccess.Write, FileShare.Read)

                delta.ApplyAsync(basisStream, new BinaryDeltaReader(deltaStream, progressReporter), newFileStream)
                |> Async.AwaitTask
                |> Async.RunSynchronously

                Ok(newFile)
            with

            e ->
                try
                    Shell.cleanDir <| getBaseUpdatePath()
                with _ -> ()
                Error(e.Message)

        /// Apply the new patch
        let applyPatch (path: string) =
            let fi = FileInfo.ofPath path

            let newName =
                if Environment.isLinux then fi.Name
                else "installer.exe"

            try
                if fi.Exists then
                    let origFile = getOriginal()
                    Shell.rm (origFile.FullName)
                    Shell.cp fi.FullName (origFile.Directory.FullName @@ newName)
                    if Environment.isLinux then Linux.Utils.setMBHome (origFile.Directory.FullName @@ newName)
                    Shell.cleanDir <| getBaseUpdatePath()
                    Ok <| Some(origFile.Directory.FullName @@ newName)
                else failwithf "Update file not found %s" path
            with e -> Error e.Message

        /// Try to clean update path and dispose of errors
        let cleanBaseUpdatePath() =
            try
                Shell.cleanDir <| getBaseUpdatePath()
            with e ->
#if DEBUG
                failwith e.Message
#endif
                ()

        /// Start the new application
        let startNewVersion (file: string) =
            async {
                let fi = FileInfo.ofPath file
                Process.shellExec { ExecParams.Empty with Program = fi.FullName } |> ignore
            }
