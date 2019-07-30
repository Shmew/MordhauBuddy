namespace MordhauBuddy.App

module Entry =
    open Elmish
    open Elmish.React
    open Elmish.Debug
    open Elmish.HMR
    open Elmish.Bridge

    Program.mkProgram App.init App.update App.view
    |> Program.withBridgeConfig(
        Bridge.endpoint ("http://localhost:8085/ws")
        |> Bridge.withName "testBridge"
        |> Bridge.withUrlMode UrlMode.Raw)
    |> Program.withReactSynchronous "app"
#if DEBUG
    |> Program.withDebugger
    |> Program.withConsoleTrace
#endif
    |> Program.run
