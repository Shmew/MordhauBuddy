namespace MordhauBuddy.Core

open Fake.Core
open Fake.IO
open Fake.IO.FileSystemOperators
open INIReader
open INIReader.INIExtensions.Options
open MordhauBuddy.Shared.ElectronBridge
open System

/// Operations on Mordhau Game ini
module INIConfiguration =
    /// Module for doing file operations
    module FileOps =
        /// Try to find the configuration directory
        let defConfigDir =
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

        let tryGetFile (iFile : INIFile) =
            let fiPath = IO.FileInfo(iFile.File).FullName
            match iFile.WorkingDir, (fiPath = iFile.File && File.exists (iFile.File)) with
            | _, true -> Some(iFile.File)
            | Some(dir), _ when File.exists (dir @@ iFile.File) -> Some(dir @@ iFile.File)
            | _ -> None

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
                    File.readAsString file |> INIValue.TryParse
                with _ -> None
            else None

    /// Module for modifying character face values
    module Frankenstein =
        [<RequireQualifiedAccess>]
        type FaceActions =
            | Frankenstein
            | Random
            | Custom of FaceValues

        let max = 65535
        let min = 0
        let rng = System.Random()

        let faceKeys =
            [ [ "FaceCustomization"; "Translate" ]
              [ "FaceCustomization"; "Rotate" ]
              [ "FaceCustomization"; "Scale" ] ]

        /// Generate random values and map over `int list`
        let randomFaces() =
            [ 0..48 ]
            |> List.map (fun _ ->
                   rng.Next(min, max)
                   |> string
                   |> Some
                   |> INIValue.String)
            |> INIValue.Tuple

        /// Randomly assign max or min values over `int list`
        let frankensteinFaces() =
            [ 0..48 ]
            |> List.map (fun _ ->
                   if rng.Next(min, 1) = 1 then max
                   else min
                   |> string
                   |> Some
                   |> INIValue.String)
            |> INIValue.Tuple

        let modifyFace (profile : INIValue) (f : unit -> INIValue) =
            faceKeys |> List.fold (fun (p : INIValue) sels -> p.Map(sels, f())) profile

        let modifyCustomFace (profile : INIValue) (fVals : FaceValues) =
            faceKeys
            |> List.zip ([ fVals.Translate; fVals.Rotate; fVals.Scale ]
                         |> List.map (fun iList ->
                                iList
                                |> List.map (string
                                             >> Some
                                             >> INIValue.String)
                                |> INIValue.Tuple))
            |> List.fold (fun (p : INIValue) (fValues, sels) -> p.Map(sels, fValues)) profile

        let private getProps s (iVal : INIValue) = iVal.TryGetProperty(s)

        let getPropValuesOf (gameFile : INIValue) (selectors : string list) =
            selectors
            |> List.fold (fun (acc : INIValue list list) elem ->
                   acc
                   |> List.map (fun iList ->
                          iList
                          |> List.map (fun iVal ->
                                 match getProps elem iVal with
                                 | Some(res) -> res
                                 | _ -> [])
                          |> List.concat)) [ [ gameFile ] ]
            |> List.concat

        let private iValStrings (iVal : INIValue) =
            match iVal with
            | INIValue.String(Some(s)) -> s.Trim('"')
            | _ -> ""

        let getCharacterProfileNames (gameFile : INIValue) =
            [ "File"; @"/Game/Mordhau/Blueprints/BP_MordhauSingleton.BP_MordhauSingleton_C"; "CharacterProfiles"; "Name";
              "INVTEXT" ]
            |> getPropValuesOf gameFile
            |> List.map iValStrings

        let containsProfile (gameFile : INIValue) (profile : string) =
            match getCharacterProfileNames gameFile with
            | s when s |> List.contains profile -> true
            | _ -> false

        let modifyCharacterProfileFace (gameFile : INIValue) (profile : string) (action : FaceActions) =
            let charProfileSelectors =
                [ "File"; @"/Game/Mordhau/Blueprints/BP_MordhauSingleton.BP_MordhauSingleton_C"; "CharacterProfiles" ]
            let containsProf = getCharacterProfileNames (gameFile) |> List.contains (profile)

            let newFace (iVal : INIValue) =
                match action with
                | FaceActions.Frankenstein -> modifyFace iVal frankensteinFaces
                | FaceActions.Random -> modifyFace iVal randomFaces
                | FaceActions.Custom(fValues) -> modifyCustomFace iVal fValues

            let hasProfile (iVal : INIValue) =
                [ "Name"; "INVTEXT" ]
                |> getPropValuesOf iVal
                |> List.map iValStrings
                |> List.contains profile

            if containsProf then
                charProfileSelectors
                |> getPropValuesOf gameFile
                |> List.filter hasProfile
                |> List.tryHead
                |> Option.bind (newFace
                                >> (fun iVal -> gameFile.MapIf(charProfileSelectors, iVal, hasProfile))
                                >> Some)
            else None

    let modifySectionSettings (engineFile : INIValue) (section : string) (filters : string list) (iVal : INIValue) =
        engineFile.Map(section :: filters, iVal)

    let addSectionSettings (engineFile : INIValue) (section : string) (values : INIValue list) =
        match engineFile.TryGetProperty(section) with
        | Some(s) ->
            s
            |> List.append values
            |> List.sort
            |> (fun newVal -> engineFile.Map([ section ], INIValue.Section(section, newVal)))
        | None ->
            (section, values |> List.sort) :: engineFile.Properties
            |> List.map INIValue.Section
            |> INIValue.File

    module BridgeOperations =
        open Frankenstein

        let replace (oVal : INIValue) (iVal : INIValue) (selectors : string list) = oVal.Map(selectors, iVal)
        let delete (oVal : INIValue) (selectors : string list) = oVal.Map(selectors, INIValue.String(None))
        let exists (iFile : INIFile) = FileOps.tryGetFile(iFile).IsSome |> BridgeResult.Exists
        let parse (iFile : INIFile) = FileOps.tryGetFile (iFile) |> Option.bind FileOps.tryReadINI

        let backup (iFile : INIFile) =
            FileOps.tryGetFile (iFile)
            |> Option.map FileOps.createBackup
            |> function
            | Some(b) when b -> b
            | _ -> false
            |> BridgeResult.Backup

        let defDir() = FileOps.defConfigDir |> BridgeResult.DefaultDir
        let random (profile : string) (iVal : INIValue) = modifyCharacterProfileFace iVal profile FaceActions.Random
        let frankenstein (profile : string) (iVal : INIValue) =
            modifyCharacterProfileFace iVal profile FaceActions.Frankenstein
        let custom (profile : string) (iVal : INIValue) (fVals : FaceValues) =
            modifyCharacterProfileFace iVal profile <| FaceActions.Custom(fVals)
        let profileList (iVal : INIValue) = getCharacterProfileNames iVal |> BridgeResult.ProfileList
