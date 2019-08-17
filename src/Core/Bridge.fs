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

        let update (clientDispatch : Dispatch<RemoteClientMsg>) (ClientMsg clientMsg) (model : Model) =
            let updateModel (file : File) (iOpt : INIValue option) model =
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
                            updateModel iFile.File result model,
                            result.IsSome
                            |> BridgeResult.Replace
                            |> createClientResp caller (Some(iFile.File))
                        | Delete(sels, iFile) ->
                            let result = (delete (model.GetIVal(iFile.File).Value) sels.Selectors |> Some)
                            updateModel iFile.File result model,
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
                            updateModel iFile.File result model,
                            result.IsSome
                            |> BridgeResult.Parse
                            |> createClientResp caller (Some(iFile.File))
                        | Backup(fList) ->
                            model,
                            fList
                            |> List.map backup
                            |> List.forall id
                            |> BridgeResult.Backup
                            |> createClientResp caller None
                        | DefaultDir ->
                            model,
                            defDir()
                            |> BridgeResult.DefaultDir
                            |> createClientResp caller None
                        | Commit(fList) ->
                            model,
                            fList
                            |> List.map (fun iFile -> write iFile (model.GetIVal(iFile.File).Value))
                            |> List.forall id
                            |> BridgeResult.CommitChanges
                            |> createClientResp caller None
                    | Faces fCmd ->
                        let cResp br = createClientResp caller (Some(File.Game)) br
                        match fCmd with
                        | Random(profiles) ->
                            let result = random profiles (model.GetIVal(File.Game).Value)
                            updateModel File.Game result model, result.IsSome |> FaceResult.Random
                        | Frankenstein(profiles) ->
                            let result = frankenstein profiles (model.GetIVal(File.Game).Value)
                            updateModel File.Game result model, result.IsSome |> FaceResult.Frankenstein
                        | Custom(profiles, fVal) ->
                            let result = custom profiles (model.GetIVal(File.Game).Value) fVal
                            updateModel File.Game result model, result.IsSome |> FaceResult.Custom
                        | ProfileList -> model, profileList (model.GetIVal(File.Game).Value) |> FaceResult.ProfileList
                        |> fun (m, fr) ->
                            m,
                            fr
                            |> BridgeResult.Faces
                            |> cResp
                    | Configs cCmd ->
                        let cResp br = createClientResp caller None br
                        let engine = model.GetIVal(File.Engine).Value
                        let gameUser = model.GetIVal(File.GameUserSettings).Value
                        match cCmd with
                        | GetConfigs(oList) ->
                            model,
                            oList
                            |> getConfigs engine gameUser
                            |> ConfigResult.GetConfigs
                        | MapConfigs(oList) ->
                            let newEngine, newGameUser = oList |> mapConfigs engine gameUser
                            model |> (updateModel File.Engine newEngine >> updateModel File.Game newGameUser),
                            (newEngine.IsSome && newGameUser.IsSome) |> ConfigResult.MapConfigs
                        |> fun (m, cr) ->
                            m,
                            cr
                            |> BridgeResult.Config
                            |> cResp

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
