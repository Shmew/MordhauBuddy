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
        open BlackFox.CommandLine
        open Fake.Windows
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

        let private regBase = Registry.RegistryBaseKey.HKEYCurrentUser
        let private regKey = @"Software\Microsoft\Windows\CurrentVersion\Run"
        let private regSubValue = "MordhauBuddy"

        /// Try to locate the current executable directory
        let private appPath = Environment.CurrentDirectory

        /// Get executable path
        let private execPath =
            Environment.SpecialFolder.LocalApplicationData
            |> Environment.GetFolderPath
            |> fun d -> (new IO.DirectoryInfo(d)).FullName
            |> fun d -> (d @@ "mordhau-buddy-updater" @@ "installer.exe")

        /// Try to get linux home path
        let private homePath =
            if Environment.isLinux then tryGetEnvVar "HOME"
            else None

        /// Get application version
        let private version =
            Assembly.GetExecutingAssembly().GetName().Version |> (fun v -> sprintf "%i.%i.%i" v.Major v.Minor v.Build)

        /// Get the name of the AppImage
        let private appImageName = sprintf "MordhauBuddy-%s.AppImage" version

        /// Create desktop file string
        let private desktopFile() =
            { Name = "MordhauBuddy"
              Comment = "Compilation of Mordhau Tools"
              Version = version
              Type = "Application"
              Terminal = false
              Exec = (appPath @@ sprintf "MordhauBuddy-%s.AppImage" version)
              Icon =
                  (defaultArg (homePath |> Option.map (fun p -> p @@ ".local/share/MordhauBuddy")) appPath @@ "icon.png") }
            |> fun dFile -> dFile.ToDesktopString()

        /// Registers the application on linux
        let registerLinuxApp() =
            async {
                try
                    if Environment.isLinux then
                        homePath
                        |> Option.map (fun path -> path @@ ".local/share")
                        |> function
                        | Some(path) ->
                            Directory.ensure (path @@ "MordhauBuddy")
                            File.writeString false (path @@ "applications/mordhau-buddy.desktop") <| desktopFile()
                            Shell.AsyncExec(appImageName, args = " --appimage-extract static/icon.png", dir = appPath)
                            |> withTimeout 10000
                            |> Async.Ignore
                            |> Async.Start
                            Async.Sleep 10000 |> Async.RunSynchronously
                            try
                                try
                                    Shell.moveFile (path @@ "MordhauBuddy") (appPath @@ "squashfs-root/static/icon.png")
                                    Async.Sleep 6000 |> Async.RunSynchronously
                                with _ -> ()
                            finally
                                Shell.deleteDir (appPath @@ "squashfs-root")
                        | None -> ()
                with _ -> ()
            }
            |> Async.Start

        /// Try to enable auto launch
        let enableAutoLaunch() =
            let applyDesktopFile dir =
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

            if Environment.isLinux then
                async {
                    return try
                               homePath
                               |> Option.map (fun path -> applyDesktopFile (path @@ ".config"))
                               |> Option.isSome
                           with _ -> false
                }
                |> Async.RunSynchronously
            else
                async {
                    return try
                               use registryKey = Registry.getRegistryKey regBase regKey true
                               registryKey.SetValue(regSubValue, execPath, Microsoft.Win32.RegistryValueKind.String)
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
                           if Environment.isLinux then
                               homePath
                               |> Option.map (fun path -> path @@ ".config/autostart/mordhau-buddy.desktop")
                               |> function
                               | Some(path) ->
                                   File.delete path
                                   File.exists appPath
                               | None -> false
                           else
                               Registry.deleteRegistryValue regBase regKey regSubValue
                               Registry.valueExistsForKey regBase regKey regSubValue
                           |> not
                       with _ -> false
            }
            |> Async.RunSynchronously
