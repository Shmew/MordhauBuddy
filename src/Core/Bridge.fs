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
            { Game: INIValue option
              Engine: INIValue option
              GameUserSettings: INIValue option }
            member this.GetIVal(file: ConfigFile) =
                match file with
                | ConfigFile.Game -> this.Game
                | ConfigFile.Engine -> this.Engine
                | ConfigFile.GameUserSettings -> this.GameUserSettings

        type ServerMsg = ClientMsg of RemoteServerMsg

        let init (clientDispatch: Dispatch<RemoteClientMsg>) () =
            Connected |> clientDispatch
            { Game = None
              GameUserSettings = None
              Engine = None }, Cmd.none

        let createClientResp (caller: Caller) (file: INIFile option) (br: BridgeResult) =
            { Caller = caller
              File = file
              BridgeResult = br }

        let update (clientDispatch: Dispatch<RemoteClientMsg>) (ClientMsg clientMsg) (model: Model) =
            let updateModel (file: ConfigFile) (iOpt: INIValue option) model =
                match file with
                | ConfigFile.Game -> { model with Game = iOpt }
                | ConfigFile.GameUserSettings -> { model with GameUserSettings = iOpt }
                | ConfigFile.Engine -> { model with Engine = iOpt }

            let model, remoteCMsg =
                match clientMsg with
                | INIOps(ops, caller) ->
                    match ops with
                    | Operation iCmd ->
                        match iCmd with
                        | Replace(s, sels, iFile) ->
                            let result =
                                (replace (model.GetIVal(iFile.File).Value)
                                     (s
                                      |> Some
                                      |> INIValue.String) sels.Selectors
                                 |> Some)
                            updateModel iFile.File result model,
                            result.IsSome
                            |> BridgeResult.Replace
                            |> createClientResp caller (Some(iFile))
                        | Delete(sels, iFile) ->
                            let result = (delete (model.GetIVal(iFile.File).Value) sels.Selectors |> Some)
                            updateModel iFile.File result model,
                            result.IsSome
                            |> BridgeResult.Delete
                            |> createClientResp caller (Some(iFile))
                        | Exists(iFile) ->
                            model,
                            exists iFile
                            |> BridgeResult.Exists
                            |> createClientResp caller (Some(iFile))
                        | MapDirExists(dir) ->
                            model,
                            mapDirExists dir
                            |> BridgeResult.MapDirExists
                            |> createClientResp caller None
                        | Parse(iFile) ->
                            let result = parse iFile

                            let m, cmd =
                                updateModel iFile.File result model,
                                result.IsSome
                                |> BridgeResult.Parse
                                |> createClientResp caller (Some(iFile))
                            match iFile.File with
                            | ConfigFile.Game ->
                                { cmd with Caller = Caller.FaceTools }
                                |> Resp
                                |> clientDispatch
                            | ConfigFile.Engine when model.GameUserSettings.IsSome ->
                                { cmd with Caller = Caller.MordhauConfig }
                                |> Resp
                                |> clientDispatch
                            | ConfigFile.GameUserSettings when model.Engine.IsSome ->
                                { cmd with Caller = Caller.MordhauConfig }
                                |> Resp
                                |> clientDispatch
                            | _ -> ()
                            m, cmd
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
                        | DefaultMapDir ->
                            model,
                            defMapDir()
                            |> BridgeResult.DefaultMapDir
                            |> createClientResp caller None
                        | Commit(fList) ->
                            model,
                            fList
                            |> List.map (fun iFile -> write iFile (model.GetIVal(iFile.File).Value))
                            |> List.forall id
                            |> BridgeResult.CommitChanges
                            |> createClientResp caller None
                    | Faces fCmd ->
                        let cResp br = createClientResp caller None br
                        match fCmd with
                        | Random(profiles) ->
                            let result = random profiles (model.GetIVal(ConfigFile.Game).Value)
                            updateModel ConfigFile.Game result model, result.IsSome |> FaceResult.Random
                        | Frankenstein(profiles) ->
                            let result = frankenstein profiles (model.GetIVal(ConfigFile.Game).Value)
                            updateModel ConfigFile.Game result model, result.IsSome |> FaceResult.Frankenstein
                        | Custom(profiles, fVal) ->
                            let result = custom profiles (model.GetIVal(ConfigFile.Game).Value) fVal
                            updateModel ConfigFile.Game result model, result.IsSome |> FaceResult.Custom
                        | ProfileList ->
                            model, profileList (model.GetIVal(ConfigFile.Game).Value) |> FaceResult.ProfileList
                        |> fun (m, fr) ->
                            m,
                            fr
                            |> BridgeResult.Faces
                            |> cResp
                    | Configs cCmd ->
                        let cResp br = createClientResp caller None br
                        let engine = model.GetIVal(ConfigFile.Engine).Value
                        let gameUser = model.GetIVal(ConfigFile.GameUserSettings).Value
                        match cCmd with
                        | GetConfigs(oList) ->
                            model,
                            oList
                            |> getConfigs engine gameUser
                            |> ConfigResult.GetConfigs
                        | MapConfigs(oList) ->
                            let newEngine, newGameUser = oList |> mapConfigs engine gameUser
                            model
                            |> (updateModel ConfigFile.Engine newEngine >> updateModel ConfigFile.Game newGameUser),
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
