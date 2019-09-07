namespace MordhauBuddy.Core

open MordhauBuddy.Shared.ElectronBridge

/// Mapping of internal functions to client requests
module BridgeOperations =
    /// Community related bridge commands
    [<RequireQualifiedAccess>]
    module Community =
        open Http.WebRequests

        /// Try to get a list of stream announcements for Mordhau
        let getSteamAnn() =
            match tryGetSteamAnnRSS() with
            | Some(sList) -> sList
            | _ -> []

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
        let replace (oVal: INIValue) (iVal: INIValue) (selectors: string list) = oVal.Map(selectors, iVal)

        /// Delete contents of oVal that match selectors
        let delete (oVal: INIValue) (selectors: string list) = oVal.Map(selectors, INIValue.String(None))

        /// Determine if a file exists
        let exists (iFile: INIFile) = FileOps.INI.tryGetFile iFile.File.Name iFile.WorkingDir |> Option.isSome

        /// Try to open and parse a ini file
        let parse (iFile: INIFile) =
            FileOps.INI.tryGetFile iFile.File.Name iFile.WorkingDir |> Option.bind FileOps.INI.tryReadINI

        /// Write an `INIValue` to a file, overwriting if it already exists
        let write (iFile: INIFile) (iVal: INIValue) =
            FileOps.INI.tryGetFile iFile.File.Name iFile.WorkingDir
            |> Option.bind (FileOps.INI.writeINI iVal)
            |> Option.isSome

        /// Create a backup of a file by putting it into a subdirectory
        let backup (iFile: INIFile) =
            FileOps.INI.tryGetFile iFile.File.Name iFile.WorkingDir
            |> Option.map FileOps.INI.createBackup
            |> function
            | Some(b) when b -> b
            | _ -> false

        /// Clean backup files based on given policy
        let cleanBackups (bSet: BackupSettings) (iFile: INIFile) =
            FileOps.INI.tryGetFile iFile.File.Name iFile.WorkingDir
            |> Option.map (FileOps.INI.cleanBackups bSet)
            |> ignore

        /// Ranomize the profiles if they are within the given `INIValue`
        let random (profiles: string list) (iVal: INIValue) = tryApplyChanges profiles iVal FaceActions.Random

        /// Frankenstein the profiles if they are within the given `INIValue`
        let frankenstein (profiles: string list) (iVal: INIValue) =
            tryApplyChanges profiles iVal FaceActions.Frankenstein

        /// Set the profiles to a custom import string if they are within the given `INIValue`
        let custom (profiles: string list) (iVal: INIValue) (fVals: string) =
            tryApplyChanges profiles iVal <| FaceActions.Custom(fVals)

        /// Get the profiles within the given `INIValue`
        let profileList (iVal: INIValue) = getCharacterProfileNames iVal |> getCharacterProfileExports iVal

        /// Get the configurations set in the two INIValues if present
        let getConfigs (engine: INIValue) (gameUser: INIValue) (options: OptionGroup list) =
            getSettings engine gameUser options

        /// Apply the new values into the two INIValues or add them
        let mapConfigs (engine: INIValue) (gameUser: INIValue) (options: OptionGroup list) =
            tryMapSettings engine gameUser options

    /// Map related bridge commands
    [<RequireQualifiedAccess>]
    module Maps =
        open Helpers
        open Http
        open Http.WebRequests
        open System
        open System.Threading

        /// Try to locate the default Mordhau maps directory
        let defDir() = FileOps.Maps.defaultDir

        /// Determine if input is valid maps directory
        let dirExists (dir: string) = FileOps.Maps.tryFindMaps dir

        /// Get the list of valid community maps based on info files
        let getAvailableMaps() =
            let infoArr (s: string) = s.Split([| "\r\n"; "\r"; "\n" |], StringSplitOptions.None)
            match getInfoFiles() with
            | Ok(resList) ->
                resList
                |> List.choose (function
                    | Some(infoF) as i when (infoArr infoF).Length >= 5 -> i
                    | _ -> None)
            | Error(_) -> []

        /// Get all installed maps
        let getInstalledMaps (dir: string) = FileOps.Maps.getInstalled dir

        /// Download map if available
        let installMap (mCmd: MapTarget) (dispatchWrapper: MapResult -> unit) (cToken: CancellationToken) =
            let fName = mCmd.Folder + ".zip"
            let getMap (mList: GHContents list) = mList |> List.filter (fun m -> m.Name = fName)
            let diDir = IO.DirectoryInfo(mCmd.Directory)
            match mCmd.GDrive with
            | Some(gd) ->
                async {
                    { Url = sprintf "https://www.googleapis.com/drive/v3/files/%s" gd.ID
                      FileName = fName
                      MapName = mCmd.Folder
                      Directory = diDir
                      Size = gd.Size
                      UpdateFun = fun i -> MapResult.InstallMapProgress(mCmd.Folder, i) |> dispatchWrapper
                      CompleteFun = fun () -> MapResult.InstallMapComplete(mCmd.Folder) |> dispatchWrapper
                      ErrorFun = fun e -> MapResult.InstallMapError(mCmd.Folder, e) |> dispatchWrapper
                      CancelFun = fun c -> MapResult.InstallMapCancelled(mCmd.Folder, true) |> dispatchWrapper }
                    |> downloadGDFile cToken
                }
                |> Async.Start
                true
            | _ ->
                match getMapFiles() with
                | Ok(mList) when (mList
                                  |> getMap
                                  |> List.isEmpty
                                  |> not)
                                 && diDir.Exists ->
                    async {
                        let fileInfo =
                            mList
                            |> getMap
                            |> List.head
                        { Url = fileInfo.DownloadUrl
                          FileName = fName
                          MapName = mCmd.Folder
                          Directory = diDir
                          Size =
                              fileInfo.Size
                              |> float
                              |> (*) 1.0<B>
                              |> convertBtoMB
                          UpdateFun = fun i -> MapResult.InstallMapProgress(mCmd.Folder, i) |> dispatchWrapper
                          CompleteFun = fun () -> MapResult.InstallMapComplete(mCmd.Folder) |> dispatchWrapper
                          ErrorFun = fun e -> MapResult.InstallMapError(mCmd.Folder, e) |> dispatchWrapper
                          CancelFun = fun c -> MapResult.InstallMapCancelled(mCmd.Folder, true) |> dispatchWrapper }
                        |> downloadFile cToken
                    }
                    |> Async.Start
                    true
                | _ -> false

        /// Remove a map
        let uninstallMap (dir: string) (fName: string) = FileOps.Maps.tryUninstall dir fName

    /// Settings related bridge commands
    [<RequireQualifiedAccess>]
    module Settings =
        open FileOps.AutoLaunch

        /// Try to enable the auto launch
        let enableAutoLaunch() = enableAutoLaunch()

        /// Try to disable the auto launch
        let disableAutoLaunch launchEnv = disableAutoLaunch launchEnv

        /// Try to setup the linux application
        let setupLinux() = registerLinuxApp()
