namespace MordhauBuddy.Core

open MordhauBuddy.Shared.ElectronBridge

/// Mapping of internal functions to client requests
module BridgeOperations =
    /// INI related bridge commands
    [<RequireQualifiedAccess>]
    module INI =
        open INIReader
        open INIReader.INIExtensions.Options
        open INIConfiguration
        open Frankenstein
        open MordhauConfig

        /// Try to locate the default Mordhau configuration directory
        let defDir() = FileOps.INI.defaultDir

        /// Replace the oVal with iVal based on selectors
        let replace (oVal : INIValue) (iVal : INIValue) (selectors : string list) = oVal.Map(selectors, iVal)

        /// Delete contents of oVal that match selectors
        let delete (oVal : INIValue) (selectors : string list) = oVal.Map(selectors, INIValue.String(None))

        /// Determine if a file exists
        let exists (iFile : INIFile) = FileOps.INI.tryGetFile iFile.File.Name iFile.WorkingDir |> Option.isSome

        /// Try to open and parse a ini file
        let parse (iFile : INIFile) =
            FileOps.INI.tryGetFile iFile.File.Name iFile.WorkingDir |> Option.bind FileOps.INI.tryReadINI

        /// Write an `INIValue` to a file, overwriting if it already exists
        let write (iFile : INIFile) (iVal : INIValue) =
            FileOps.INI.tryGetFile iFile.File.Name iFile.WorkingDir
            |> Option.bind (FileOps.INI.writeINI iVal)
            |> Option.isSome

        /// Create a backup of a file by putting it into a subdirectory
        let backup (iFile : INIFile) =
            FileOps.INI.tryGetFile iFile.File.Name iFile.WorkingDir
            |> Option.map FileOps.INI.createBackup
            |> function
            | Some(b) when b -> b
            | _ -> false

        /// Ranomize the profiles if they are within the given `INIValue`
        let random (profiles : string list) (iVal : INIValue) = tryApplyChanges profiles iVal FaceActions.Random

        /// Frankenstein the profiles if they are within the given `INIValue`
        let frankenstein (profiles : string list) (iVal : INIValue) =
            tryApplyChanges profiles iVal FaceActions.Frankenstein

        /// Set the profiles to a custom import string if they are within the given `INIValue`
        let custom (profiles : string list) (iVal : INIValue) (fVals : string) =
            tryApplyChanges profiles iVal <| FaceActions.Custom(fVals)

        /// Get the profiles within the given `INIValue`
        let profileList (iVal : INIValue) = getCharacterProfileNames iVal |> getCharacterProfileExports iVal

        /// Get the configurations set in the two INIValues if present
        let getConfigs (engine : INIValue) (gameUser : INIValue) (options : OptionGroup list) =
            getSettings engine gameUser options

        /// Apply the new values into the two INIValues or add them
        let mapConfigs (engine : INIValue) (gameUser : INIValue) (options : OptionGroup list) =
            tryMapSettings engine gameUser options

    /// Map related bridge commands
    [<RequireQualifiedAccess>]
    module Maps =
        open Maps
        open Maps.WebRequests
        open System

        /// Try to locate the default Mordhau maps directory
        let defDir() = FileOps.Maps.defaultDir

        /// Determine if input is valid maps directory
        let dirExists (dir : string) = FileOps.Maps.tryFindMaps dir

        /// Get the list of valid community maps based on info files
        let getAvailableMaps() =
            let infoArr (s : string) = s.Split([| "\r\n"; "\r"; "\n" |], StringSplitOptions.None)
            match getInfoFiles() with
            | Ok(resList) ->
                resList
                |> List.choose (function
                       | Ok(infoF) when (infoArr infoF).Length >= 9 -> Some(infoF)
                       | _ -> None)
            | Error(e) -> []

        /// Get all installed maps
        let getInstalledMaps (dir : string) = FileOps.Maps.getInstalled dir

        /// Download map if available
        let installMap (mCmd : MapTarget) (dispatchWrapper : MapResult -> unit) =
            let fName = mCmd.Folder + ".zip"
            let getMap (mList : GHTypes.GHContents list) = mList |> List.filter (fun m -> m.Name = fName)
            let diDir = IO.DirectoryInfo(mCmd.Directory)
            match mCmd.GDrive with
            | Some(gd) ->
                { Url = sprintf "https://www.googleapis.com/drive/v3/files/%s" gd.ID
                  FileName = fName
                  Directory = diDir
                  Size = gd.Size
                  UpdateFun = fun i -> MapResult.InstallMapProgress(mCmd.Folder, i) |> dispatchWrapper
                  CompleteFun = fun () -> MapResult.InstallMapComplete(mCmd.Folder) |> dispatchWrapper
                  ErrorFun = fun e -> MapResult.InstallMapError(mCmd.Folder, e) |> dispatchWrapper
                  CancelFun = fun c -> MapResult.InstallMapCancelled(mCmd.Folder, true) |> dispatchWrapper }
                |> downloadGDFile
                true
            | _ ->
                match getMapFiles() with
                | Ok(mList) when (mList
                                  |> getMap
                                  |> List.length > 0)
                                 && diDir.Exists ->
                    let fileInfo =
                        mList
                        |> getMap
                        |> List.head
                    { Url = fileInfo.DownloadUrl
                      FileName = fName
                      Directory = diDir
                      Size =
                          Math.DivRem(fileInfo.Size, 1000000L)
                          |> fun (i1, i2) -> sprintf "%i.%i" i1 i2
                          |> float
                          |> (*) 1.0<MB>
                      UpdateFun = fun i -> MapResult.InstallMapProgress(mCmd.Folder, i) |> dispatchWrapper
                      CompleteFun = fun () -> MapResult.InstallMapComplete(mCmd.Folder) |> dispatchWrapper
                      ErrorFun = fun e -> MapResult.InstallMapError(mCmd.Folder, e) |> dispatchWrapper
                      CancelFun = fun c -> MapResult.InstallMapCancelled(mCmd.Folder, true) |> dispatchWrapper }
                    |> downloadFile
                    true
                | _ -> false
