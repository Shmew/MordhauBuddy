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
                let ts = DateTime.Now.ToString("yyyyMMdd-hhmm") + ".ini"
                Directory.ensure backups
                Shell.copyFile (backups @@ ts) file
                File.exists (backups @@ ts)
            | false -> false

        /// Write `INIValue` to file path
        let writeINI (iVal : INIValue) (outFile : string) =
            let fi = FileInfo.ofPath (outFile)
            Directory.ensure fi.DirectoryName
            File.writeString false fi.FullName (iVal.ToString())
            tryGetFile ({ File = outFile
                          WorkingDir = None })

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
            | Custom of string

        let max = 65535
        let min = 0
        let rng = System.Random()

        let faceKeys =
            [ [ "CharacterProfiles"; "FaceCustomization"; "Translate" ]
              [ "CharacterProfiles"; "FaceCustomization"; "Rotate" ]
              [ "CharacterProfiles"; "FaceCustomization"; "Scale" ] ]

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

        let mapProps (s: string) (iList: INIValue list) =
            iList
            |> List.collect (fun iVal -> iVal.TryGetProperty s |> Option.defaultValue [])

        let modifyFace (profile : INIValue) (f : unit -> INIValue) =
            faceKeys |> List.fold (fun (p : INIValue) sels -> p.Map(sels, f())) profile
        let private getProps s (iVal : INIValue) = iVal.TryGetProperty(s)

        let getPropValuesOf (gameFile : INIValue) (selectors : string list) =
            selectors
            |> List.fold (fun (acc : INIValue list list) elem ->
                   acc
                   |> List.map (fun iList ->
                          iList
                          |> List.collect (fun iVal ->
                                 match getProps elem iVal with
                                 | Some(res) -> res
                                 | _ -> []))) [ [ gameFile ] ]
            |> List.concat

        let private iValStrings (iVal : INIValue) =
            match iVal with
            | INIValue.Tuple(tList) when tList.Length = 1 ->
                match tList.Head with
                | INIValue.String(Some(s)) -> s.Trim('"')
                | _ -> ""
            | INIValue.String(Some(s)) -> s.Trim('"')
            | _ -> ""

        let getCharacterProfileNames (gameFile : INIValue) =
            [ "File"; @"/Game/Mordhau/Blueprints/BP_MordhauSingleton.BP_MordhauSingleton_C"; "CharacterProfiles"; "Name";
              "INVTEXT" ]
            |> getPropValuesOf gameFile
            |> List.map iValStrings

        let getCharacterProfileExports (gameFile : INIValue) (profiles : string list) =
            let characterIVals =
                [ "File"; @"/Game/Mordhau/Blueprints/BP_MordhauSingleton.BP_MordhauSingleton_C" ]
                |> getPropValuesOf gameFile

            let checkVList (vList : INIValue list) (profile : string) =
                vList
                |> List.tryFind (fun iVal ->
                       match iVal with
                       | INIValue.KeyValue("Name", fText) ->
                           match fText with
                           | INIValue.FieldText(_, INIValue.Tuple(tList)) when profile = (tList.Head.ToString()
                                                                                               .Trim('"')) -> true
                           | _ -> false
                       | _ -> false)
                |> Option.isSome
            profiles
            |> List.map
                ((fun profile ->
                   let exportList =
                       characterIVals
                       |> List.choose (fun iElem ->
                              match iElem with
                              | INIValue.KeyValue("CharacterProfiles", INIValue.Tuple(vList)) when checkVList vList
                                                                                                       profile ->
                                  Some(iElem)
                              | _ -> None)
                   match exportList with
                   | [ charIVal ] -> profile, Some(charIVal)
                   | _ -> profile, Some(INIValue.String(None)))
                >> (fun (pName, iValOpt) ->
                   match iValOpt with
                   | Some(iVal) ->
                       match iVal with
                       | INIValue.KeyValue("CharacterProfiles", INIValue.Tuple(tList)) ->
                           tList
                           |> List.choose (fun tElem ->
                                  match tElem with
                                  | INIValue.KeyValue("FaceCustomization", fVal) -> Some(pName, fVal.ToString())
                                  | _ -> None)
                           |> function
                           | [ single ] -> single
                           | _ -> (pName, "")
                       | _ -> (pName, "")
                   | None -> (pName, "")))

        let modifyCharacterProfileFace (gameFile : INIValue) (profile : string) (action : FaceActions) =
            let newFace (iVal : INIValue) =
                match action with
                | FaceActions.Frankenstein -> modifyFace iVal frankensteinFaces
                | FaceActions.Random -> modifyFace iVal randomFaces
                | FaceActions.Custom(fValues) -> modifyFace iVal (fun () -> INIValue.Parse(fValues))

            let charProfiles,otherItems =
                [gameFile]
                |> mapProps "File"
                |> mapProps @"/Game/Mordhau/Blueprints/BP_MordhauSingleton.BP_MordhauSingleton_C"
                |> List.partition (fun iVal -> iVal.TryGetProperty("CharacterProfiles").IsSome)

            charProfiles
            |> List.map (fun iVal ->
                match iVal.TryGetProperty("CharacterProfiles") with
                | Some(iList) -> 
                    iList 
                | None -> [])
            |> List.filter (List.isEmpty >> not)
            |> List.map 
                ((fun iList -> INIValue.KeyValue("CharacterProfiles",INIValue.Tuple(iList)))
                >>
                (fun iVal ->
                    match iVal?CharacterProfile?Name?INVTEXT.AsString() with
                    | Some(s) when s = profile ->
                        newFace iVal
                    | _ -> iVal))
            |> fun cProfs ->
                let h,t =
                    otherItems
                    |> List.splitAt 1
                cProfs
                |> List.append h
                |> List.append t
            |> fun iList -> INIValue.Section(@"/Game/Mordhau/Blueprints/BP_MordhauSingleton.BP_MordhauSingleton_C", iList)
            |> fun section -> gameFile.Map([ @"/Game/Mordhau/Blueprints/BP_MordhauSingleton.BP_MordhauSingleton_C" ], section)

    module BridgeOperations =
        open Frankenstein

        let replace (oVal : INIValue) (iVal : INIValue) (selectors : string list) = oVal.Map(selectors, iVal)
        let delete (oVal : INIValue) (selectors : string list) = oVal.Map(selectors, INIValue.String(None))
        let exists (iFile : INIFile) = FileOps.tryGetFile(iFile).IsSome |> BridgeResult.Exists
        let parse (iFile : INIFile) = FileOps.tryGetFile (iFile) |> Option.bind FileOps.tryReadINI

        let write (iFile : INIFile) (iVal : INIValue) =
            FileOps.tryGetFile (iFile)
            |> Option.bind (FileOps.writeINI iVal)
            |> Option.isSome
            |> BridgeResult.CommitChanges

        let backup (iFile : INIFile) =
            FileOps.tryGetFile (iFile)
            |> Option.map FileOps.createBackup
            |> function
            | Some(b) when b -> b
            | _ -> false
            |> BridgeResult.Backup

        let defDir() = FileOps.defConfigDir |> BridgeResult.DefaultDir

        let private tryApplyChanges (profiles : string list) (iVal : INIValue) (fAction : FaceActions) =
            try
                profiles
                |> List.fold (fun acc profile -> modifyCharacterProfileFace acc profile fAction) iVal
                |> Some
            with _ -> None

        let random (profiles : string list) (iVal : INIValue) = tryApplyChanges profiles iVal FaceActions.Random
        let frankenstein (profiles : string list) (iVal : INIValue) =
            tryApplyChanges profiles iVal FaceActions.Frankenstein
        let custom (profiles : string list) (iVal : INIValue) (fVals : string) =
            tryApplyChanges profiles iVal <| FaceActions.Custom(fVals)

        let profileList (iVal : INIValue) =
            getCharacterProfileNames iVal
            |> getCharacterProfileExports iVal
            |> BridgeResult.ProfileList
