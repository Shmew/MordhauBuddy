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

        let tryGetFile (iFile: INIFile) =
            match iFile.WorkingDir,File.exists(iFile.File) with
            | _, true -> Some(iFile.File)
            | Some(dir),_ when File.exists(dir @@ iFile.File) 
                -> Some(dir @@ iFile.File)
            | None,_ when defConfigDir.IsSome && File.exists(defConfigDir.Value @@ iFile.File) 
                -> Some(defConfigDir.Value @@ iFile.File)
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
            [ ["FaceCustomization"; "Translate"]
              ["FaceCustomization"; "Rotate"]
              ["FaceCustomization"; "Scale"] ]

        /// Generate random values and map over `int list`
        let randomFaces () =
            [0..48] 
            |> List.map (fun _ ->
                rng.Next(min, max) 
                |> string 
                |> Some 
                |> INIValue.String) 
            |> INIValue.Tuple

        /// Randomly assign max or min values over `int list`
        let frankensteinFaces () =
            [0..48]
            |> List.map (fun _ ->
                if rng.Next(min, 1) = 1 then max
                else min
                |> string
                |> Some
                |> INIValue.String)
            |> INIValue.Tuple

        let modifyFace (profile: INIValue) (f: unit -> INIValue) =
            faceKeys
            |> List.fold (fun (p: INIValue) sels ->
                p.Map(sels,f())) profile

        let modifyCustomFace (profile: INIValue) (fVals: FaceValues) =
            faceKeys
            |> List.zip (fVals.getTuples)
            |> List.fold (fun (p: INIValue) (fValues, sels) ->
                p.Map(sels,fValues)) profile

        let getPropValuesOf (gameFile: INIValue) (selectors: string list) =
            let getProps s (iVal: INIValue) =
                iVal.TryGetProperty(s)
            
            selectors 
            |> List.mapFold (fun (acc: INIValue list) s ->
            let props = acc |> List.choose (getProps s)
            props.IsEmpty,(props |> List.concat)
            ) [gameFile]
            |> (fun (_,iList) -> 
            iList 
            |> List.choose (fun i -> match i with | INIValue.String(s) -> s | _ -> None))

        let getCharacterProfileNames (gameFile : INIValue) =
            [@"/Game/Mordhau/Blueprints/BP_MordhauSingleton.BP_MordhauSingleton_C"; "CharacterProfiles"; "Name"; "INVTEXT"]
            |> getPropValuesOf gameFile

        let containsProfile (gameFile : INIValue) (profile: string) =
            match getCharacterProfileNames gameFile with
            | s when s |> List.contains profile -> true
            | _ -> false

        let modifyCharacterProfileFace (gameFile : INIValue) (profile: string) (action: FaceActions) =
            if containsProfile gameFile profile then
                let profiles =
                    [@"/Game/Mordhau/Blueprints/BP_MordhauSingleton.BP_MordhauSingleton_C"; "CharacterProfiles"]
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

        let mapBool (b: bool) =
            if b then BridgeResult.Success 
            else BridgeResult.Failure

        let mapIOption (o: INIValue option) =
            match o with
            | Some(v) -> v |> BridgeResult.IValue
            | None -> BridgeResult.Failure

        let mapSOption (s: string option) =
            match s with
            | Some(v) -> v |> BridgeResult.Text
            | None -> BridgeResult.Failure 

        let replace (oVal: INIValue) (iVal: INIValue) (selectors: string list) =
            oVal.Map(selectors,iVal)
            |> BridgeResult.IValue

        let delete (oVal: INIValue) (selectors: string list) =
            oVal.Map(selectors,INIValue.String(None))
            |> BridgeResult.IValue

        let exists (iFile: INIFile) =
            FileOps.tryGetFile(iFile).IsSome |> mapBool

        let parse (iFile: INIFile) =
            FileOps.tryGetFile(iFile) 
            |> Option.bind FileOps.tryReadINI
            |> mapIOption

        let backup (iFile: INIFile) =
            FileOps.tryGetFile(iFile)
            |> Option.map FileOps.createBackup
            |> function
            | Some(b) when b -> b
            | _ -> false
            |> mapBool

        let defDir () =
            FileOps.defConfigDir
            |> mapSOption

        let random (profile: string) (iVal: INIValue) =
            modifyCharacterProfileFace iVal profile FaceActions.Random
            |> mapIOption

        let frankenstein (profile: string) (iVal: INIValue) =
            modifyCharacterProfileFace iVal profile FaceActions.Frankenstein
            |> mapIOption

        let custom (profile: string) (iVal: INIValue) (fVals: FaceValues) =
            modifyCharacterProfileFace iVal profile <| FaceActions.Custom(fVals)
            |> mapIOption

        let profileList (iVal: INIValue) =
            getCharacterProfileNames iVal
            |> BridgeResult.TextList