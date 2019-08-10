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

    Program.mkProgram State.init State.update View.view
    |> Program.withBridgeConfig(
        Bridge.endpoint (endpoint + INIOperations.Endpoint)
        |> Bridge.withName "INI"
        |> Bridge.withMapping (fun bridgeMsg ->
            bridgeMsg |> Types.Msg.ServerMsg)
        |> Bridge.withUrlMode UrlMode.Raw
        |> Bridge.withRetryTime 15 
        |> Bridge.withWhenDown (Disconnected |> Types.ServerMsg))
    |> Program.withReactSynchronous "app"
#if DEBUG
    |> Program.withDebugger
    |> Program.withConsoleTrace
#endif
    |> Program.run
