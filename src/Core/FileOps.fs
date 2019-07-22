namespace MordhauBuddy.Core

open Fake.Core
open Fake.IO
open Fake.IO.FileSystemOperators
open INIReader
open System

/// Module for doing file operations
module FileOps =
    /// Try to find the configuration directory
    let defConfigDir =
        let mordPath = "Mordhau/Saved/Config/WindowsClient"

        let bindDirectory (dir : string) =
            dir
            |> IO.DirectoryInfo
            |> DirectoryInfo.exists
            |> function
            | true -> Some(dir)
            | false -> None
        match Environment.isWindows with
        | true ->
            Environment.SpecialFolder.LocalApplicationData
            |> Environment.GetFolderPath
            |> (fun fol -> fol @@ mordPath)
            |> bindDirectory
        | false ->
            [ "~/.steam/steam"; "~/.local/share/Steam" ]
            |> List.tryFind (bindDirectory >> Option.isSome)
            |> Option.bind
                   (fun dir -> dir @@ "steamapps/compatdata/629760/pfx/drive_c/Users/steamuser/AppData/Local" |> Some)
            |> Option.bind bindDirectory

    /// Create a backup of the given file into sub directory MordhauBuddy_backups
    let createBackup (file : string) =
        let fi = FileInfo.ofPath (file)
        match File.exists file with
        | true ->
            let backups = fi.DirectoryName @@ "MordhauBuddy_backups"
            let ts = DateTime.Now.ToString("yyyyMMdd-hhmm")
            Directory.ensure backups
            Shell.copyFile (backups @@ ts) file
            File.exists (backups @@ ts @@ (fi.Name))
        | false -> false

    /// Write `INIValue` to file path
    let writeINI (iVal : INIValue) (outFile : string) =
        let fi = FileInfo.ofPath (outFile)
        Directory.ensure fi.DirectoryName
        File.writeString false fi.FullName (iVal.ToString())

    /// Try to read an INI file
    let tryReadINI (file : string) =
        if File.exists file then
            try
                File.readAsString file |> INIReader.INIValue.TryParse
            with _ -> None
        else None
