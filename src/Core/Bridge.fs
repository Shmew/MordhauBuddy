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
            { Game : INIValue option
              GameUserSettings : INIValue option
              Engine : INIValue option }
            member this.GetIVal(file : File) =
                match file with
                | File.Game -> this.Game
                | File.GameUserSettings -> this.GameUserSettings
                | File.Engine -> this.Engine

        type ServerMsg = ClientMsg of RemoteServerMsg

        let init (clientDispatch : Dispatch<RemoteClientMsg>) () =
            Connected |> clientDispatch
            { Game = None
              GameUserSettings = None
              Engine = None }, Cmd.none

        let createClientResp (caller : Caller) (file : File option) (br : BridgeResult) =
            { Caller = caller
              File = file
              BridgeResult = br }

        let update (clientDispatch : Dispatch<RemoteClientMsg>) (ClientMsg clientMsg) model =
            let updateModel (file : File) (iOpt : INIValue option) =
                match file with
                | File.Game -> { model with Game = iOpt }
                | File.GameUserSettings -> { model with GameUserSettings = iOpt }
                | File.Engine -> { model with Engine = iOpt }

            let model, remoteCMsg =
                match clientMsg with
                | INIOps(ops, caller) ->
                    match ops with
                    | Operation iCmd ->
                        match iCmd with
                        | Replace(s, sels, iFile) ->
                            let result =
                                (replace (model.GetIVal(iFile.File).Value) (s
                                                                            |> Some
                                                                            |> INIValue.String) sels.Selectors
                                 |> Some)
                            updateModel iFile.File result,
                            result.IsSome
                            |> BridgeResult.Replace
                            |> createClientResp caller (Some(iFile.File))
                        | Delete(sels, iFile) ->
                            let result = (delete (model.GetIVal(iFile.File).Value) sels.Selectors |> Some)
                            updateModel iFile.File result,
                            result.IsSome
                            |> BridgeResult.Delete
                            |> createClientResp caller (Some(iFile.File))
                        | Exists(iFile) ->
                            model,
                            exists iFile
                            |> BridgeResult.Exists
                            |> createClientResp caller (Some(iFile.File))
                        | Parse(iFile) ->
                            let result = parse iFile
                            updateModel iFile.File result,
                            result.IsSome
                            |> BridgeResult.Parse
                            |> createClientResp caller (Some(iFile.File))
                        | Backup(iFile) ->
                            model,
                            backup iFile
                            |> BridgeResult.Backup
                            |> createClientResp caller (Some(iFile.File))
                        | DefaultDir ->
                            model,
                            defDir()
                            |> BridgeResult.DefaultDir
                            |> createClientResp caller None
                        | Commit(iFile) ->
                            model,
                            write iFile (model.GetIVal(iFile.File).Value)
                            |> BridgeResult.CommitChanges
                            |> createClientResp caller (Some(iFile.File))
                    | Faces fCmd ->
                        let cResp br = createClientResp caller (Some(File.Game)) br
                        match fCmd with
                        | Random(profiles) ->
                            let result = random profiles (model.GetIVal(File.Game).Value)
                            updateModel File.Game result, result.IsSome |> BridgeResult.Random
                        | Frankenstein(profiles) ->
                            let result = frankenstein profiles (model.GetIVal(File.Game).Value)
                            updateModel File.Game result, result.IsSome |> BridgeResult.Frankenstein
                        | Custom(profiles, fVal) ->
                            let result = custom profiles (model.GetIVal(File.Game).Value) fVal
                            updateModel File.Game result, result.IsSome |> BridgeResult.Custom
                        | ProfileList -> model, profileList (model.GetIVal(File.Game).Value) |> BridgeResult.ProfileList
                        |> fun (m, br) -> m, cResp br

            Resp(remoteCMsg) |> clientDispatch
            model, Cmd.none

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
