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
        | GetMapDir
        | SetConfigDir of string * Result<string, string list> * ConfigFile
        | SetMapDir of string * Result<string, string list>
        | RequestLoad of DirLoad
        | LoadCanceled
        | MapUpdateSetting of UpdateSettings option
        | BackupSetting of BackupSettings option

    type Model =
        { EngineDir: ConfigDir
          GameDir: ConfigDir
          GameUserDir: ConfigDir
          MapsDir: ConfigDir
          MapUpdateSettings: UpdateSettings
          BackupSettings: BackupSettings }
