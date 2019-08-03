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

        let getPropValuesOf (gameFile : INIValue) (selectors : string list) = /// DEBUG THIS, returning empty list in snd tuple value
            let getProps s (iVal : INIValue) = iVal.TryGetProperty(s)
            selectors
            |> List.mapFold (fun (acc : INIValue list) s ->
                   let props = acc |> List.choose (getProps s)
                   props.IsEmpty, (props |> List.concat)) [ gameFile ]
            |> (fun (_, iList) ->
            iList
            |> List.choose (fun i ->
                   match i with
                   | INIValue.String(s) -> s
                   | _ -> None))

        let getCharacterProfileNames (gameFile : INIValue) =
            //let getProps s (iVal : INIValue) = iVal.TryGetProperty(s)
            //[ @"/Game/Mordhau/Blueprints/BP_MordhauSingleton.BP_MordhauSingleton_C"; "Name";
            //  "INVTEXT" ] //getPropValuesOf gameFile
            gameFile?``/Game/Mordhau/Blueprints/BP_MordhauSingleton.BP_MordhauSingleton_C``
            |> Option.map (fun res -> res |> List.map (fun r -> r?Name))
            |> Option.get
            |> List.choose id
            |> List.concat
            |> List.choose (fun res ->
                   match res with
                   | INIValue.FieldText("INVTEXT", INIValue.Tuple(s)) ->
                       s
                       |> List.choose (fun i ->
                              match i with
                              | INIValue.String(sOpt) -> sOpt
                              | _ -> None)
                       |> Some
                   | _ -> None)
            |> List.concat
            |> List.map (fun s -> s.Trim('"'))

        //|> Option.get |> List.choose id
        //) [ gameFile ]
        //getPropValuesOf gameFile [ @"/Game/Mordhau/Blueprints/BP_MordhauSingleton.BP_MordhauSingleton_C"]//; "Name"]//; "INVTEXT" ]
        let containsProfile (gameFile : INIValue) (profile : string) =
            match getCharacterProfileNames gameFile with
            | s when s |> List.contains profile -> true
            | _ -> false

        let modifyCharacterProfileFace (gameFile : INIValue) (profile : string) (action : FaceActions) =
            if containsProfile gameFile profile then
                let profiles =
                    [ @"/Game/Mordhau/Blueprints/BP_MordhauSingleton.BP_MordhauSingleton_C"; "CharacterProfiles" ]
                    |> getPropValuesOf gameFile
                profiles
                |> List.find (fun s -> s.Contains(profile))
                |> INIValue.Parse
                |> (fun iVal ->
                match action with
                | FaceActions.Frankenstein -> modifyFace iVal frankensteinFaces
                | FaceActions.Random -> modifyFace iVal randomFaces
                | FaceActions.Custom(fValues) -> modifyCustomFace iVal fValues)
                |> Some
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
