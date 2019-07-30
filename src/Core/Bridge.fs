namespace MordhauBuddy.Core

module Bridge =
    open Elmish
    open Giraffe
    open Elmish.Bridge
    open FSharp.Control.Tasks.V2
    open Saturn
    open MordhauBuddy.Shared.ElectronBridge

    type ServerState = Nothing

    type ServerMsg = ClientMsg of RemoteServerMsg

    let testHub = ServerHub().RegisterServer(ClientMsg)
    let init (clientDispatch : Dispatch<RemoteClientMsg>) () = Nothing, Cmd.none

    let update (clientDispatch : Dispatch<RemoteClientMsg>) (ClientMsg clientMsg) currentState =
        match clientMsg with
        | Text s ->
            clientDispatch (Resp("clientDispatch!"))
            currentState, Cmd.none
        | Close -> currentState, Cmd.none

    let bridge =
        Bridge.mkServer socketPath init update
        |> Bridge.withConsoleTrace
        |> Bridge.withServerHub testHub
        |> Bridge.run Giraffe.server

    let server =
        router {
            get "/api/helloworld" (fun next ctx -> task { return! Successful.OK "Hello world" next ctx })
            get "/ws" bridge
        }

    let app =
        application {
            use_router server
            disable_diagnostics
            app_config Giraffe.useWebSockets
            url (sprintf "http://0.0.0.0:%i/" port)
        }

    [<EntryPoint>]
    let main _ =
        printfn "Working directory - %s" (System.IO.Directory.GetCurrentDirectory())
        run app
        0 // return an integer exit code
