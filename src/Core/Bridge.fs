namespace MordhauBuddy.Core

module Bridge =
    open Elmish
    open Elmish.Bridge
    open Saturn
    open INIConfiguration.BridgeOperations
    open MordhauBuddy.Shared.ElectronBridge

    module INIBridge =
        type ServerState = Nothing

        type ServerMsg = ClientMsg of RemoteServerMsg

        let hub = ServerHub().RegisterServer(ClientMsg)
        let init (clientDispatch : Dispatch<RemoteClientMsg>) () = Nothing, Cmd.none

        let update (clientDispatch : Dispatch<RemoteClientMsg>) (ClientMsg clientMsg) currentState =
            match clientMsg with
            | INIOps ops ->
                match ops with
                | Operation iCmd ->
                    match iCmd with
                    | Replace(oVal, iVal, sels) -> replace oVal iVal sels.Selectors
                    | Delete(oVal, sels) -> delete oVal sels.Selectors
                    | Exists(iFile) -> exists iFile
                    | Parse(iFile) -> parse iFile
                    | Backup(iFile) -> backup (iFile)
                    | DefaultDir -> defDir()
                | Faces fCmd ->
                    match fCmd with
                    | Random(profile, iVal) -> random profile iVal
                    | Frankenstein(profile, iVal) -> frankenstein profile iVal
                    | Custom(profile, iVal, fVal) -> custom profile iVal fVal
                    | ProfileList(iVal) -> profileList iVal
            |> Resp
            |> clientDispatch
            currentState, Cmd.none

        let bridge =
            Bridge.mkServer INIOperations.Endpoint init update
            |> Bridge.withConsoleTrace
            |> Bridge.withServerHub hub
            |> Bridge.run Giraffe.server

    let server = router { get INIOperations.Endpoint INIBridge.bridge }

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
