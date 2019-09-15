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
    open RenderUtils
    open RenderUtils.Directory
    open MordhauBuddy.Shared.ElectronBridge
    open Microsoft.FSharp.Reflection

    type Msg =
        | ClientMsg of BridgeMsg
        | GetDefaultDir
        | SetConfigDir of string * Result<string, string list> * ConfigFile
        | RequestLoad of ConfigFile
        | LoadCanceled
        | BackupSetting of BackupSettings option
        | ToggleAutoLaunch
        | RunSetup

    type Model =
        { EngineDir: ConfigDir
          GameDir: ConfigDir
          GameUserDir: ConfigDir
          BackupSettings: BackupSettings
          AutoLaunch: bool }
