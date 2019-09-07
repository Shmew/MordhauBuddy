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
            else
                None

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
        open Helpers.Json
        open System.Reflection


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

        /// Get application version
        let private version =
            Assembly.GetExecutingAssembly().GetName().Version |> (fun v -> sprintf "%i.%i.%i" v.Major v.Minor v.Build)

        [<RequireQualifiedAccess>]
        module Windows =

            [<AutoOpen>]
            module private Utils =
                let regBase = Registry.RegistryBaseKey.HKEYCurrentUser
                let regKey = @"Software\Microsoft\Windows\CurrentVersion\Run"
                let regSubValue = "MordhauBuddy"

                /// Get Windows executable path
                let getWinExecPath() =
                    Environment.SpecialFolder.LocalApplicationData
                    |> Environment.GetFolderPath
                    |> fun d -> (new IO.DirectoryInfo(d)).FullName
                    |> fun d -> (d @@ "mordhau-buddy-updater" @@ "installer.exe")

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
            module private Utils =
                /// Try to get home path
                let homePath = tryGetEnvVar "HOME"

                /// Get the name of the AppImage
                let appImageName = sprintf "MordhauBuddy-%s.AppImage" version

                /// Gets the home share falling back to `appPath` if necessary
                let homeShare = defaultArg (homePath |> Option.map (fun p -> p @@ ".local/share/mordhau-buddy")) appPath

                /// Gets or sets the MordhauBuddy env var if necessary
                let getMBEnv() =
                    match FileInfo.ofPath (homeShare @@ "home.json") with
                    | hShare when hShare.Exists ->
                        File.readAsString hShare.FullName
                        |> Json.deserializeEx<HomeFile> config
                        |> fun hf -> hf.Path
                    | _ ->
                        let ePath = appPath @@ appImageName
                        { Path = ePath }
                        |> Json.serializeEx config
                        |> File.writeString false (homeShare @@ "home.json")
                        ePath

                /// Create desktop file string
                let desktopFile() =
                    { Name = "MordhauBuddy"
                      Comment = "Compilation of Mordhau Tools"
                      Version = version
                      Type = "Application"
                      Terminal = false
                      Exec = getMBEnv()
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
