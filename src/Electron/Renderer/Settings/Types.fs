namespace MordhauBuddy.App.Settings

module Types =
    open System
    open Fable.Core
    open Fable.React
    open Fable.React.Props
    open Fable.MaterialUI
    open Fable.MaterialUI.Core
    open FSharp.Core  /// To avoid shadowing Result<_,_>
    open MordhauBuddy.App
    open RenderUtils
    open RenderUtils.Directory
    open MordhauBuddy.Shared.ElectronBridge
    open Microsoft.FSharp.Reflection

    type Msg =
        | ClientMsg of BridgeMsg
        | Save
        | GetDefaultDir
        | GetMapDir
        | SetConfigDir of string * Result<string,string list> * ConfigFile
        | SetMapDir of string * Result<string,string list>
        | RequestLoad of DirLoad
        | LoadCanceled
        | SnackMsg of Snackbar.Types.Msg<Msg>
        | SnackDismissMsg

    type Save =
        { Waiting : bool
          Error : bool
          HelperText : string
          Complete : bool }

    type Model = 
        { Waiting : bool
          Complete : bool
          EngineDir : ConfigDir
          GameDir : ConfigDir
          GameUserDir : ConfigDir 
          MapsDir : ConfigDir
          Save : Save 
          Snack : Snackbar.Types.Model<Msg> }
