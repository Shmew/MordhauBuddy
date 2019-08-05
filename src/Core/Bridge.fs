namespace MordhauBuddy.Core

module Bridge =
    open Elmish
    open Elmish.Bridge
    open Saturn
    open INIReader
    open INIConfiguration.BridgeOperations
    open MordhauBuddy.Shared.ElectronBridge

    module INIBridge =
        type Model =
            { IValue : INIValue option }

        type ServerMsg = ClientMsg of RemoteServerMsg

        let init (clientDispatch : Dispatch<RemoteClientMsg>) () = { IValue = None }, Cmd.none

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
                    | Exists(iFile) -> model, exists iFile
                    | Parse(iFile) ->
                        match parse iFile with
                        | Some(_) as iOpt -> { model with IValue = iOpt }, BridgeResult.Parse true
                        | _ -> model, BridgeResult.Parse false
                    | Backup(iFile) -> model, backup iFile
                    | DefaultDir -> model, defDir()
                    | Commit(iFile) -> model, write iFile (model.IValue.Value)
                | Faces fCmd ->
                    match fCmd with
                    | Random(profiles) ->
                        { model with IValue = (random profiles model.IValue.Value) }, true |> BridgeResult.Random
                    | Frankenstein(profiles) ->
                        { model with IValue = (frankenstein profiles model.IValue.Value) },
                        true |> BridgeResult.Frankenstein
                    | Custom(profiles, fVal) ->
                        { model with IValue = (custom profiles model.IValue.Value fVal) }, true |> BridgeResult.Custom
                    | ProfileList -> model, profileList (model.IValue.Value)
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
