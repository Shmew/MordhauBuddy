namespace MordhauBuddy.Core

open MordhauBuddy.Shared.ElectronBridge

/// Mapping of internal functions to client requests
module BridgeOperations =

    /// Misc bridge commands
    [<RequireQualifiedAccess>]
    module Misc =
        open Helpers

        /// Check if Mordhau is running
        let isMordhauRunning() = async { return Info.isMordhauRunning() } |> Async.RunSynchronously

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
            |> Option.bind (FileOps.INI.tryWriteINI iVal)
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
        let getConfigs (engine: INIValue) (gameUser: INIValue) (input: INIValue) (options: OptionGroup list) =
            getSettings engine gameUser input options

        /// Apply the new values into the two INIValues or add them
        let mapConfigs (engine: INIValue) (gameUser: INIValue) (input: INIValue) (options: OptionGroup list) =
            tryMapSettings engine gameUser input options

    /// Mod related bridge commands
    [<RequireQualifiedAccess>]
    module Mods =
        open Fake.IO.FileSystemOperators
        open Helpers
        open Http
        open Http.WebRequests
        open System
        open System.Threading
               
        /// Try to locate the default Mordhau mods directory
        let defDir() = FileOps.Mods.defaultDir

        /// Determine if input is valid maps directory
        let dirExists (dir: string) = FileOps.Mods.tryFindMods dir

        /// Get the list of valid community maps based on info files
        let getAvailableMods() = getInfoFiles()

        /// Get all installed maps
        let getInstalledMods (dir: string) = FileOps.Mods.getInstalled dir

        /// Download map if available
        let installMod (mCmd: ModTarget) (dispatchWrapper: ModResult -> unit) (cToken: CancellationToken) =
            let cacheFolder = Info.updaterPath "Mods" @@ (string mCmd.ModInfo.ModId)
            let fName = (string mCmd.ModInfo.ModId)  + ".zip"
            let diDir = IO.DirectoryInfo(mCmd.Directory)
            
            async {
                { Url = mCmd.ModInfo.FileUrl
                  FileName = fName
                  Name = cacheFolder
                  CacheDirectory = IO.DirectoryInfo(cacheFolder)
                  Directory = diDir
                  ModInfo = mCmd.ModInfo
                  Size =
                      mCmd.ModInfo.Size
                      |> Option.defaultValue 0
                      |> float
                      |> (*) 1.0<B>
                      |> convertBtoMB
                  UpdateFun = fun i -> ModResult.InstallModProgress(mCmd.ModInfo.ModId, i) |> dispatchWrapper
                  CompleteFun = fun () -> ModResult.InstallModComplete(mCmd.ModInfo.ModId) |> dispatchWrapper
                  ErrorFun = fun e -> ModResult.InstallModError(mCmd.ModInfo.ModId, e) |> dispatchWrapper
                  CancelFun = fun c -> ModResult.InstallModCancelled(mCmd.ModInfo.ModId, true) |> dispatchWrapper }
                |> downloadZipFile cToken
            }
            |> Async.Start
            true

        /// Remove a mod
        let uninstallMod (dir: string) (modId: int) = FileOps.Mods.tryUninstall dir modId

        /// Disable a mod
        let disableMod (dir: string) (modId: int) = FileOps.Mods.tryDisable dir modId

        /// Enable a mod
        let enableMod (dir: string) (modId: int) = FileOps.Mods.tryEnable dir modId

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

    [<RequireQualifiedAccess>]
    module Updating =
        open FileOps.Updating
        open Http.WebRequests

        /// Downloads and prepares for update
        let getUpdates() =
            async {
                return getReleases()
                       |> Result.bind tryGetAssets
                       |> function
                       | Ok(valid) ->
                           match valid with
                           | Zero -> Ok None
                           | One nUp ->
                               match isReady (nUp.LatestRel.TagName) with
                               | Some(r) -> Ok <| Some(r)
                               | _ ->
                                   nUp
                                   |> getAsset
                                   |> Result.bind tryGeneratePatch
                                   |> function
                                   | Ok f -> Ok <| Some f
                                   | Error e -> Error e
                           | Multiple nUp ->
                               match isReady (nUp.LatestRel.TagName) with
                               | Some(r) -> Ok <| Some(r)
                               | _ ->
                                   nUp
                                   |> getAsset
                                   |> function
                                   | Ok fi -> Ok <| Some fi.FullName
                                   | Error e -> Error e
                       | Error e -> Error e
            }
            |> Async.RunSynchronously

        /// Install the new update
        let installUpdates (newFile: string) = async { return applyPatch newFile } |> Async.RunSynchronously

        /// Clean the Updating temp directory
        let cleanUpdatingDir() = async { cleanBaseUpdatePath() } |> Async.RunSynchronously

        /// Launch the new version
        let launchNewVersion (file: string) =
            async {
                startNewVersion (file) |> Async.Start
                do! Async.Sleep 1000
            }
            |> Async.RunSynchronously
