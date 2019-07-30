namespace MordhauBuddy.Core

module ElectronBridge =
    type BridgeMsg =
        | SomeMsg
        | Text of string
        | Close

    let port = "8085" |> uint16
    let endpoint = sprintf "http://localhost:%i" port
    let socketPath = "/ws"

module Bridge =
    open Elmish
    open Giraffe
    open Elmish.Bridge
    open FSharp.Control.Tasks.V2
    open Saturn
    open ElectronBridge

    type Model =
        | Connected
        | Disconnected

    let init (clientDispatch : Dispatch<BridgeMsg>) () =
        clientDispatch (Text "I came from the server!")
        Connected, Cmd.none

    let update clientDispatch msg model =
        match msg with
        | Text(s) ->
            System.Console.WriteLine(s)
            clientDispatch (Text <| sprintf "I came from the update func on server! %s" s)
            Connected, Cmd.none
        | Close -> Disconnected, Cmd.none

    let bridge =
        Bridge.mkServer socketPath init update
        |> Bridge.withConsoleTrace
        //|> Bridge.withServerHub hub
        |> Bridge.run Giraffe.server

    //let hub = ServerHub<State,ServerMsg,OuterMsg>()
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
