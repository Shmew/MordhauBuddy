namespace MordhauBuddy.App.Settings

module State =
    open FSharp.Core  // To avoid shadowing Result<_,_>
    open MordhauBuddy.App
    open RenderUtils
    open RenderUtils.Validation
    open Elmish
    open Elmish.Bridge
    open MordhauBuddy.Shared.ElectronBridge
    open BridgeUtils
    open RenderUtils.Directory
    open Types

    let init() =
        { Waiting = true }

    let update (msg: Msg) (model: Model) =
        match msg with
        | _ -> model,Cmd.none
