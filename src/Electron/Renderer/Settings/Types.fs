namespace MordhauBuddy.App.Settings

module Types =
    open System
    open Fable.Core
    open Fable.React
    open Fable.React.Props
    open Fable.MaterialUI
    open Fable.MaterialUI.Core
    open FSharp.Core /// To avoid shadowing Result<_,_>
    open MordhauBuddy.App
    open BridgeUtils
    open RenderUtils
    open RenderUtils.Directory
    open MordhauBuddy.Shared.ElectronBridge
    open Microsoft.FSharp.Reflection

    type Msg =
        | ClientMsg of BridgeMsg
        | GetDefaultDir
        | GetModDir
        | SetConfigDir of string * Result<string, string list> * ConfigFile
        | SetModDir of string * Result<string, string list>
        | RequestLoad of DirLoad
        | LoadCanceled
        | ModUpdateSettings of UpdateSettings option
        | BackupSetting of BackupSettings option
        | ToggleAutoLaunch
        | RunSetup

    type Model =
        { EngineDir: ConfigDir
          GameDir: ConfigDir
          GameUserDir: ConfigDir
          InputDir: ConfigDir
          ModsDir: ConfigDir
          ModUpdateSettings: UpdateSettings
          BackupSettings: BackupSettings
          AutoLaunch: bool }
