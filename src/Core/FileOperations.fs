namespace MordhauBuddy.Core

open Fake.Core
open Fake.IO
open Fake.IO.FileSystemOperators
open Helpers
open INIReader
open System

/// Module for doing file operations
module FileOps =
    /// File operations for INI files
    module INI =
        open MordhauBuddy.Shared.ElectronBridge

        let logger = Logger "FileOps.INI"

        /// Try to find the configuration directory
        let defaultDir =
            let bindDirectory (dir: string) =
                IO.DirectoryInfo dir
                |> DirectoryInfo.exists
                |> function
                | true -> Some(dir)
                | false ->
                    logger.LogWarn "Failed to find default directory: %s does not exist." dir
                    None

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
            | _ ->
                logger.LogWarn "Unable to find INIFile %s in %O" file workingDir
                None

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
            | false ->
                logger.LogError "Tried to backup non-existant file: %s" file
                false

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
                            if k = "" then
                                ()
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
                logger.LogError "Failed to clean backups:\n%O" e

        /// Write `INIValue` to file path
        let tryWriteINI (iVal: INIValue) (outFile: string) =
            try
                let fi = FileInfo.ofPath (outFile)
                Directory.ensure fi.DirectoryName
                File.writeString false fi.FullName (iVal.ToString())
                tryGetFile outFile None
            with e ->
                logger.LogError "Failed to write INI %O to %s:\n%O" iVal outFile e
                None

        /// Try to read an INI file
        let tryReadINI (file: string) =
            if File.exists file then
                try
                    File.readAsString file |> INIValue.TryParse
                with e ->
                    logger.LogError "Failed to read INI file: %s\n%O" file e
                    None
            else
                logger.LogError "Tried to read INI file that does not exist: %s" file
                None

    /// File operations for enabling auto launching
    module AutoLaunch =
        open Fake.Windows
        open FSharp.Json
        open Helpers.Json
        open Helpers.Info

        let logger = Logger "FileOps.AutoLaunch"

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
                |> fun s ->
                    logger.LogInfo "Generated .desktop file:\n%s" s
                    s

        let private withTimeout timeout action =
            async { let! child = Async.StartChild(action, timeout)
                    return! child }


        [<RequireQualifiedAccess>]
        module Windows =

            let logger = Logger "FileOps.AutoLaunch.Windows"

            /// Try to enable auto launch
            let enableAutoLaunch() =
                async {
                    return try
                               use registryKey = Registry.getRegistryKey Windows.regBase Windows.regKey true
                               registryKey.SetValue
                                   (Windows.regSubValue, sprintf "%s /S --force-run" <| Windows.getWinExecPath(),
                                    Microsoft.Win32.RegistryValueKind.String)
                               registryKey.Flush()
                               registryKey.Dispose()
                               Async.Sleep 1000 |> Async.RunSynchronously
                               Registry.valueExistsForKey Windows.regBase Windows.regKey Windows.regSubValue
                           with e ->
                               logger.LogError "Error enabling auto launch:\n%O" e
                               false
                }
                |> Async.RunSynchronously

            /// Try to disable auto launch
            let disableAutoLaunch() =
                async {
                    return try
                               Registry.deleteRegistryValue Windows.regBase Windows.regKey Windows.regSubValue
                               Registry.valueExistsForKey Windows.regBase Windows.regKey Windows.regSubValue |> not
                           with e ->
                               logger.LogError "Error disabling auto launch:\n%O" e
                               false
                }
                |> Async.RunSynchronously

        [<RequireQualifiedAccess>]
        module Linux =

            let logger = Logger "FileOps.AutoLaunch.Linux"


            [<AutoOpen>]
            module Utils =
                /// Set the new MBHome path
                let setMBHome (newFile: string) =
                    try
                        { Path = newFile }
                        |> Json.serializeEx config
                        |> File.writeString false (Linux.homeShare @@ Linux.homeName)
                    with e ->
                        logger.LogError "Failed to set new MBHome path: %s\n%O" newFile e

                /// Gets or sets the MordhauBuddy home json if necessary
                let getMBHome() =
                    match FileInfo.ofPath (Linux.homeShare @@ Linux.homeName) with
                    | hShare when hShare.Exists ->
                        try
                            File.readAsString hShare.FullName
                            |> Json.deserializeEx<HomeFile> config
                            |> fun hf -> hf.Path
                        with e ->
                            logger.LogError "Failed to get MordhauBuddy home.json:\n%O" e
                            appPath @@ Linux.appImageName
                    | _ ->
                        let ePath = appPath @@ Linux.appImageName
                        { Path = ePath }
                        |> fun homeFile ->
                            try
                                homeFile
                                |> Json.serializeEx config
                                |> File.writeString false (Linux.homeShare @@ Linux.homeName)
                                ePath
                            with e ->
                                logger.LogError "Failed to set MordhauBuddy home.json:\n%O" e
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
                          (defaultArg (Linux.homePath |> Option.map (fun p -> p @@ ".local/share/mordhau-buddy"))
                               appPath @@ "icon.png") }
                    |> fun dFile -> dFile.ToDesktopString()

                /// Extracts the icon file from the AppImage and stores it in the linux .local folder
                let extractIcon (path: string) =
                    Shell.AsyncExec(Linux.appImageName, args = " --appimage-extract static/icon.png", dir = appPath)
                    |> withTimeout 5000
                    |> Async.Ignore
                    |> Async.RunSynchronously
                    try
                        try
                            async
                                { Shell.moveFile (path @@ "mordhau-buddy") (appPath @@ "squashfs-root/static/icon.png") }
                            |> withTimeout 5000
                            |> Async.RunSynchronously
                        with e ->
                            logger.LogError "Failed to extract AppImage icon from: %s\n%O" path e
                    finally
                        async {
                            let squash = (appPath @@ "squashfs-root")
                            try
                                Shell.deleteDir squash
                            with e ->
                                logger.LogError "Failed to delete %s after extracting AppImage icon:\n%O" squash e
                        }
                        |> withTimeout 5000
                        |> Async.RunSynchronously

                /// Create the autostart desktop file
                let createAutoStartDeskFile dir =
                    async {
                        let autoStart = (dir @@ "autostart")
                        let desktopFilePath = autoStart @@ "mordhau-buddy.desktop"
                        try
                            Directory.ensure autoStart
                            do! Async.Sleep 1000
                            File.writeString false desktopFilePath <| desktopFile()
                            do! Async.Sleep 1000
                            return File.exists desktopFilePath
                        with e ->
                            logger.LogError "Failed to create %s file in: %s\n%O" desktopFilePath autoStart e
                            return false
                    }
                    |> Async.RunSynchronously

            /// Registers the application on linux
            let registerLinuxApp() =
                async {
                    try
                        if Environment.isLinux then
                            Linux.homePath
                            |> Option.map (fun path -> path @@ ".local/share")
                            |> function
                            | Some(path) ->
                                Directory.ensure (path @@ "mordhau-buddy")
                                File.writeString false (path @@ "applications/mordhau-buddy.desktop") <| desktopFile()
                                if File.exists (path @@ "mordhau-buddy" @@ "icon.png") |> not then extractIcon path
                            | None -> ()
                    with e ->
                        logger.LogError "Failed to register application:\n%O" e
                }
                |> Async.Start

            /// Try to enable auto launch
            let enableAutoLaunch() =
                async {
                    return try
                               Linux.homePath
                               |> Option.map (fun path -> createAutoStartDeskFile (path @@ ".config"))
                               |> Option.isSome
                           with e ->
                               logger.LogError "failed to enable AutoLaunch:\n%O" e
                               false
                }
                |> Async.RunSynchronously

            /// Try to disable auto launch
            let disableAutoLaunch() =
                async {
                    return try
                               Linux.homePath
                               |> Option.map (fun path -> path @@ ".config/autostart/mordhau-buddy.desktop")
                               |> function
                               | Some(path) ->
                                   File.delete path
                                   File.exists appPath
                               | None -> false
                               |> not
                           with e ->
                               logger.LogError "Failed to disable AutoLaunch:\n%O" e
                               false
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

        let logger = Logger "FileOps.Updating"

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
                |> List.tryItem (i - 1)
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
                | _ ->
                    logger.LogError "No available updates, but detected behind one from current."
                    Error "No available updates"
            | Some 0 -> Ok Zero
            | Some i when i > 1 ->
                getRelAt 0
                |> getAssetOf None
                |> function
                | Some res -> Ok <| Multiple res
                | _ ->
                    logger.LogError "Multiple updates behind, asset not found"
                    Error "Multiple updates behind, asset not found"
            | _ ->
                logger.LogError "Unable to determine update position"
                Error "Unable to determine update position"

        /// Get base update path
        let private getBaseUpdatePath() =
            if Environment.isLinux then Linux.homeShare
            else Windows.getWinExecDir()
            |> fun s -> s @@ "Updating"

        /// Get update path and ensure it exists
        let private getUpdatePath (tagName: string) = getBaseUpdatePath() @@ tagName

        /// Download an assets list
        let private downloadAsset (asset: Github.Asset) (path: IO.DirectoryInfo) =
            try
                downloadFile (path.FullName @@ asset.Name) asset.BrowserDownloadUrl |> Ok
            with e ->
                logger.LogError "failed to download asset to: %s\n%O\n%O" path.FullName asset e
                raise e

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
                    Shell.cleanDir <| updatePath.Parent.FullName
                with e ->
                    logger.LogError "Failed to clean update path: %s\n%O" updatePath.Parent.FullName e
                Error e.Message

        /// Get the file that will be patched
        let getOriginal() =
            if Environment.isLinux then Linux.Utils.getMBHome()
            else Windows.getWinExecPath()
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

                Ok newFile
            with e ->
                logger.LogError "Failed to generate patched file:\n\tDelta:\n\t%O\n\tNew file: %s\n%O" deltaFi newFile e
                try
                    Shell.cleanDir <| getBaseUpdatePath()
                with e ->
                    logger.LogError "Failed to clean update path: %s\n%O" (getBaseUpdatePath()) e
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
                else
                    logger.LogError "Update file not found %s" path
                    Error <| sprintf "Update file not found %s" path
            with e ->
                logger.LogError "Error applying patch:\n\tPatched file: %s\n\tNew name: %s\n%O" path newName e
                Error e.Message

        /// Try to clean update path and dispose of errors
        let cleanBaseUpdatePath() =
            try
                Shell.cleanDir <| getBaseUpdatePath()
            with e ->
                logger.LogError "Failed to clean base update path:\n%O" e

        /// Start the new application
        let startNewVersion (file: string) =
            async {
                let fi = FileInfo.ofPath file
                { ExecParams.Empty with
                      Program = fi.FullName
                      CommandLine = "/S --force-run" }
                |> Process.shellExec
                |> ignore
            }
