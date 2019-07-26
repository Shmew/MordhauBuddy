namespace MordhauBuddy.App

module Entry =
    open Elmish
    open Elmish.React
    open Elmish.Debug
    open Elmish.HMR

    Program.mkProgram App.init App.update App.view
    |> Program.withReactSynchronous "app"
#if DEBUG
    |> Program.withDebugger
    |> Program.withConsoleTrace
#endif
    |> Program.run
