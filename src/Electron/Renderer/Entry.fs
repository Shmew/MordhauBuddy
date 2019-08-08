namespace MordhauBuddy.App

module Entry =
    open Elmish
    open Elmish.React
    open Elmish.Debug
#if DEBUG
    open Elmish.HMR
#endif
    open Elmish.Bridge
    open MordhauBuddy.Shared.ElectronBridge

    Program.mkProgram App.init App.update App.view
    |> Program.withBridgeConfig(
        Bridge.endpoint (endpoint + INIOperations.Endpoint)
        |> Bridge.withName "INI"
        |> Bridge.withMapping (fun bridgeMsg ->
            bridgeMsg |> App.Msg.ServerMsg)
        |> Bridge.withUrlMode UrlMode.Raw
        |> Bridge.withRetryTime 15 
        |> Bridge.withWhenDown (BridgeResult.Offline |> Resp |> App.ServerMsg))
    |> Program.withReactSynchronous "app"
#if DEBUG
    |> Program.withDebugger
    |> Program.withConsoleTrace
#endif
    |> Program.run
