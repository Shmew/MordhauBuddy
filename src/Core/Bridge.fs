namespace MordhauBuddy.Core

module Bridge =
    open Elmish
    open Elmish.Bridge
    open Saturn
    open INIReader
    open BridgeOperations
    open MordhauBuddy.Shared.ElectronBridge
    open System.Threading

    /// Websocket bridge
    module Bridge =
        type Model =
            { Game: INIValue option
              Engine: INIValue option
              GameUserSettings: INIValue option
              InstallingMaps: (string * CancellationTokenSource) list }
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
              Engine = None
              InstallingMaps = [] }, Cmd.none

        let createClientResp (caller: Caller) (file: INIFile option) (br: BridgeResult) =
            { Caller = caller
              File = file
              BridgeResult = br }
            |> Some

        let update (clientDispatch: Dispatch<RemoteClientMsg>) (ClientMsg clientMsg) (model: Model) =
            let updateModel (file: ConfigFile) (iOpt: INIValue option) model =
                match file with
                | ConfigFile.Game -> { model with Game = iOpt }
                | ConfigFile.GameUserSettings -> { model with GameUserSettings = iOpt }
                | ConfigFile.Engine -> { model with Engine = iOpt }

            let model, remoteCMsg =
                match clientMsg with
                | BridgeOps(ops, caller) ->
                    match ops with
                    | INIOperation iCmd ->
                        match iCmd with
                        | INIFileOperation.DefaultDir ->
                            model,
                            INI.defDir()
                            |> INIOperationResult.DefaultDir
                            |> BridgeResult.INIOperation
                            |> createClientResp caller None
                        | INIFileOperation.Replace(s, sels, iFile) ->
                            let result =
                                (INI.replace (model.GetIVal(iFile.File).Value)
                                     (s
                                      |> Some
                                      |> INIValue.String) sels.Selectors
                                 |> Some)
                            updateModel iFile.File result model,
                            result.IsSome
                            |> INIOperationResult.Replace
                            |> BridgeResult.INIOperation
                            |> createClientResp caller (Some(iFile))
                        | INIFileOperation.Delete(sels, iFile) ->
                            let result = (INI.delete (model.GetIVal(iFile.File).Value) sels.Selectors |> Some)
                            updateModel iFile.File result model,
                            result.IsSome
                            |> INIOperationResult.Delete
                            |> BridgeResult.INIOperation
                            |> createClientResp caller (Some(iFile))
                        | INIFileOperation.Exists(iFile) ->
                            model,
                            INI.exists iFile
                            |> INIOperationResult.Exists
                            |> BridgeResult.INIOperation
                            |> createClientResp caller (Some(iFile))
                        | INIFileOperation.Parse(iFile) ->
                            let result = INI.parse iFile

                            let m, cmd =
                                updateModel iFile.File result model,
                                result.IsSome
                                |> INIOperationResult.Parse
                                |> BridgeResult.INIOperation
                                |> createClientResp caller (Some(iFile))
                            match iFile.File with
                            | ConfigFile.Game ->
                                { cmd.Value with Caller = Caller.FaceTools }
                                |> Resp
                                |> clientDispatch
                            | ConfigFile.Engine when model.GameUserSettings.IsSome ->
                                { cmd.Value with Caller = Caller.MordhauConfig }
                                |> Resp
                                |> clientDispatch
                            | ConfigFile.GameUserSettings when model.Engine.IsSome ->
                                { cmd.Value with Caller = Caller.MordhauConfig }
                                |> Resp
                                |> clientDispatch
                            | _ -> ()
                            m, cmd
                        | INIFileOperation.Backup(fList) ->
                            model,
                            fList
                            |> List.map INI.backup
                            |> List.forall id
                            |> INIOperationResult.Backup
                            |> BridgeResult.INIOperation
                            |> createClientResp caller None
                        | INIFileOperation.Commit(fList) ->
                            model,
                            fList
                            |> List.map (fun iFile -> INI.write iFile (model.GetIVal(iFile.File).Value))
                            |> List.forall id
                            |> INIOperationResult.CommitChanges
                            |> BridgeResult.INIOperation
                            |> createClientResp caller None
                    | MapOperation mCmd ->
                        match mCmd with
                        | MapFileOperation.DefaultDir ->
                            model,
                            Maps.defDir()
                            |> MapOperationResult.DefaultDir
                            |> BridgeResult.MapOperation
                            |> createClientResp caller None
                        | MapFileOperation.DirExists dir ->
                            let result = Maps.dirExists dir

                            let m, cmd =
                                model,
                                result
                                |> MapOperationResult.DirExists
                                |> BridgeResult.MapOperation
                                |> createClientResp caller None
                            if result then
                                { cmd.Value with Caller = Caller.MapInstaller }
                                |> Resp
                                |> clientDispatch
                            m, cmd
                        | MapFileOperation.Delete(dir, fName) ->
                            model,
                            (fName, Maps.uninstallMap dir fName)
                            |> MapOperationResult.Delete
                            |> BridgeResult.MapOperation
                            |> createClientResp caller None
                    | Faces fCmd ->
                        let cResp br = createClientResp caller None br
                        match fCmd with
                        | Random(profiles) ->
                            let result = INI.random profiles (model.GetIVal(ConfigFile.Game).Value)
                            updateModel ConfigFile.Game result model, result.IsSome |> FaceResult.Random
                        | Frankenstein(profiles) ->
                            let result = INI.frankenstein profiles (model.GetIVal(ConfigFile.Game).Value)
                            updateModel ConfigFile.Game result model, result.IsSome |> FaceResult.Frankenstein
                        | Custom(profiles, fVal) ->
                            let result = INI.custom profiles (model.GetIVal(ConfigFile.Game).Value) fVal
                            updateModel ConfigFile.Game result model, result.IsSome |> FaceResult.Custom
                        | ProfileList ->
                            model, INI.profileList (model.GetIVal(ConfigFile.Game).Value) |> FaceResult.ProfileList
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
                            |> INI.getConfigs engine gameUser
                            |> ConfigResult.GetConfigs
                        | MapConfigs(oList) ->
                            let newEngine, newGameUser = oList |> INI.mapConfigs engine gameUser
                            model
                            |> (updateModel ConfigFile.Engine newEngine >> updateModel ConfigFile.Game newGameUser),
                            (newEngine.IsSome && newGameUser.IsSome) |> ConfigResult.MapConfigs
                        |> fun (m, cr) ->
                            m,
                            cr
                            |> BridgeResult.Config
                            |> cResp
                    | Maps mCmd ->
                        let cResp br = createClientResp caller None br
                        match mCmd with
                        | GetAvailableMaps ->
                            model,
                            Maps.getAvailableMaps()
                            |> MapResult.AvailableMaps
                            |> BridgeResult.Maps
                            |> cResp
                        | GetInstalledMaps dir ->
                            model,
                            Maps.getInstalledMaps dir
                            |> MapResult.InstalledMaps
                            |> BridgeResult.Maps
                            |> cResp
                        | InstallMap mCmd ->
                            let cSource = new CancellationTokenSource()

                            let dispatchWrapper (mr: MapResult) =
                                BridgeResult.Maps(mr)
                                |> createClientResp caller None
                                |> Option.get
                                |> Resp
                                |> clientDispatch
                            if Maps.installMap mCmd dispatchWrapper cSource.Token then
                                { model with InstallingMaps = ((mCmd.Folder, cSource) :: model.InstallingMaps) }, None
                            else
                                model,
                                (mCmd.Folder, Error("Unable to find map archive file."))
                                |> MapResult.InstallMap
                                |> BridgeResult.Maps
                                |> cResp
                        | ConfirmInstalled map ->
                            { model with InstallingMaps = (model.InstallingMaps |> List.filter (fun (s, _) -> s <> map)) },
                            None
                        | CancelMap fName ->
                            let toCancel, installing =
                                model.InstallingMaps |> List.partition (fun (name, _) -> fName = name)
                            toCancel |> List.iter (fun (_, cSource) -> cSource.Cancel())
                            { model with InstallingMaps = installing }, None





            match remoteCMsg with
            | Some(rMsg) -> Resp(rMsg) |> clientDispatch
            | _ -> ()
            model, Cmd.none

        let bridge =
            Bridge.mkServer BridgeOperations.Endpoint init update
            |> Bridge.withConsoleTrace
            |> Bridge.run Giraffe.server

    let server = router { get BridgeOperations.Endpoint Bridge.bridge }

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
