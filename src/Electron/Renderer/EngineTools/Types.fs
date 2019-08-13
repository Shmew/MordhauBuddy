namespace MordhauBuddy.App.EngineTools

module rec Types =
    open System
    open Fable.Core
    open Fable.React
    open Fable.React.Props
    open Fable.MaterialUI
    open Fable.MaterialUI.Core
    open FSharp.Core  /// To avoid shadowing Result<_,_>
    open MordhauBuddy.App
    open RenderUtils
    open MordhauBuddy.Shared.ElectronBridge
    open Microsoft.FSharp.Reflection

    type Msg =
        | ClientMsg of BridgeResult
        | GetDefaultDir
        | SetConfigDir of string * Result<string,string list>
        | RequestLoad
        | LoadCanceled
        | WaitingStart of Msg
        | SnackMsg of Snackbar.Types.Msg<Msg>
        | SnackDismissMsg

    type ConfigDir =
        { Directory : string
          Error : bool
          HelperText : string
          Validated : bool }

    type Submit =
        { Waiting : bool
          Error : bool
          HelperText : string
          Complete : bool }

    type Model = 
        { Waiting : bool
          ParseWaiting : bool
          StepperComplete : bool
          ConfigDir : ConfigDir 
          Submit : Submit
          Snack : Snackbar.Types.Model<Msg> }
