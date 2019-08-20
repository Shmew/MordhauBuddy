namespace MordhauBuddy.Core

open INIReader
open INIReader.INIExtensions.Options
open INIConfiguration
open MordhauBuddy.Shared.ElectronBridge

/// Mapping of internal functions to client requests
module BridgeOperations =
    open Frankenstein
    open MordhauConfig

    [<RequireQualifiedAccess>]
    module INI =
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

        /// Try to locate the default Mordhau configuration directory
        let defDir() = FileOps.INI.defaultDir

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

    [<RequireQualifiedAccess>]
    module Maps =
        /// Determine if input is valid maps directory
        let dirExists (dir: string) = FileOps.Maps.tryFindMaps dir

        /// Try to locate the default Mordhau maps directory
        let defDir() = FileOps.Maps.defaultDir
