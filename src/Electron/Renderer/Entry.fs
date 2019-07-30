namespace MordhauBuddy.App

module Entry =
    open Elmish
    open Elmish.React
    open Elmish.Debug
    open Elmish.HMR
    open Elmish.Bridge
    open MordhauBuddy.Shared.ElectronBridge

    Program.mkProgram App.init App.update App.view
    |> Program.withBridgeConfig(
        Bridge.endpoint (endpoint + socketPath)
        |> Bridge.withName "testBridge"
        |> Bridge.withMapping (fun bridgeMsg ->
            bridgeMsg |> App.Msg.ServerMsg)
        |> Bridge.withUrlMode UrlMode.Raw)
    |> Program.withReactSynchronous "app"
#if DEBUG
    |> Program.withDebugger
    |> Program.withConsoleTrace
#endif
    |> Program.run
