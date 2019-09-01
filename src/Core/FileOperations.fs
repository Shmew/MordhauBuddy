namespace MordhauBuddy.Core

open Fake.Core
open Fake.IO
open Fake.IO.FileSystemOperators
open INIReader
open System

/// Module for doing file operations
module FileOps =
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
                [ "~/.steam/steam"; "~/.local/share/Steam" ]
                |> List.tryFind (bindDirectory >> Option.isSome)
                |> Option.bind
                    (fun dir ->
                    dir
                    @@ "steamapps/compatdata/629760/pfx/drive_c/Users/steamuser/AppData/Local"
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
                                |> Array.iter (fun (i,f) -> if i > 9 then Shell.rm f.FullName)))
                | NoBackups ->
                    FileInfo.ofPath(file).Directory.GetDirectories("*MordhauBuddy_backups*")
                    |> Array.map (fun d -> d.FullName)
                    |> Shell.cleanDirs
            with
            | e ->
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
                    [ "~/.steam/steam"; "~/.local/share/Steam" ]
                    |> List.map (fun fol -> fol @@ @"Steam/steamapps/common/Mordhau/Mordhau/Content/Mordhau/Maps")

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
