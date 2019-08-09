namespace MordhauBuddy.Core

module Bridge =
    open Elmish
    open Elmish.Bridge
    open Saturn
    open INIReader
    open INIConfiguration.BridgeOperations
    open MordhauBuddy.Shared.ElectronBridge

    /// Websocket bridge
    module INIBridge =
        type Model =
            { IValue : INIValue option }

        type ServerMsg = ClientMsg of RemoteServerMsg

        let init (clientDispatch : Dispatch<RemoteClientMsg>) () = 
            Connected |> clientDispatch
            { IValue = None }, Cmd.none

        let update (clientDispatch : Dispatch<RemoteClientMsg>) (ClientMsg clientMsg) model =
            match clientMsg with
            | INIOps ops ->
                match ops with
                | Operation iCmd ->
                    match iCmd with
                    | Replace(s, sels) ->
                        { model with IValue =
                                         (replace model.IValue.Value (s
                                                                      |> Some
                                                                      |> INIValue.String) sels.Selectors
                                          |> Some) }, true |> BridgeResult.Replace
                    | Delete(sels) ->
                        { model with IValue = (delete model.IValue.Value sels.Selectors |> Some) },
                        true |> BridgeResult.Delete
                    | Exists(iFile) -> model, exists iFile |> BridgeResult.Exists
                    | Parse(iFile) ->
                        match parse iFile with
                        | Some(_) as iOpt -> { model with IValue = iOpt }, BridgeResult.Parse true
                        | _ -> model, BridgeResult.Parse false
                    | Backup(iFile) -> model, backup iFile |> BridgeResult.Backup
                    | DefaultDir -> model, defDir() |> BridgeResult.DefaultDir
                    | Commit(iFile) -> model, write iFile (model.IValue.Value) |> BridgeResult.CommitChanges
                | Faces fCmd ->
                    match fCmd with
                    | Random(profiles) ->
                        let result = random profiles model.IValue.Value
                        { model with IValue = result }, result.IsSome |> BridgeResult.Random
                    | Frankenstein(profiles) ->
                        let result = frankenstein profiles model.IValue.Value
                        { model with IValue = result }, result.IsSome |> BridgeResult.Frankenstein
                    | Custom(profiles, fVal) ->
                        let result = custom profiles model.IValue.Value fVal
                        { model with IValue = result }, result.IsSome |> BridgeResult.Custom
                    | ProfileList -> model, profileList (model.IValue.Value) |> BridgeResult.ProfileList
            |> (fun (m, br) ->
            Resp(br) |> clientDispatch
            m, Cmd.none)

        let bridge =
            Bridge.mkServer INIOperations.Endpoint init update
            |> Bridge.withConsoleTrace
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
