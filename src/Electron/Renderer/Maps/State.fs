namespace MordhauBuddy.App.Maps

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
    open Electron

    let init() =
        { Waiting = true
          MapDir = 
              { Dir = DirLoad.MapDir
                Label = ""
                Waiting = false
                Directory = ""
                Error = false
                HelperText = "" 
                Validated = false } }

    let private sender = new MapBridgeSender(Caller.MordhauConfig)

    let update (msg: Msg) (model: Model) =
        match msg with
        | OpenLink(url) ->
            renderer.shell.openExternal(url) |> Promise.start
            model, Cmd.none
