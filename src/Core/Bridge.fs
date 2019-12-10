namespace MordhauBuddy.Core

module App =
    open BridgeOperations
    open Elmish
    open Elmish.Bridge
    open Helpers
    open INIReader
    open Saturn
    open MordhauBuddy.Shared.ElectronBridge

    let logger = Logger "App"

    /// Websocket bridge
    [<AutoOpen>]
    module Bridge =

        let logger = Logger "Bridge"

        type Model =
            { Game: INIValue option
              Engine: INIValue option
              GameUserSettings: INIValue option
              BackupSettings: BackupSettings
              UpdatePending: string option }
            member this.GetIVal(file: ConfigFile) =
                match file with
                | ConfigFile.Game -> this.Game
                | ConfigFile.Engine -> this.Engine
                | ConfigFile.GameUserSettings -> this.GameUserSettings

        type ServerMsg = ClientMsg of RemoteServerMsg

        let private cDispatchWithLog (clientDispatch: Dispatch<RemoteClientMsg>) (msg: RemoteClientMsg) =
            match msg with
            | RemoteClientMsg.Resp(bMsg) when bMsg.BridgeResult = BridgeResult.Misc(MiscResult.MordhauRunning(true))
                                              || bMsg.BridgeResult = BridgeResult.Misc(MiscResult.MordhauRunning(false)) ->

                logger.LogVerbose "%O" msg
                msg
            | _ ->
                logger.LogInfo "%O" msg
                msg
            |> clientDispatch

        let private modelUpdateWithLog (oldModel: Model) (model: Model) =
            if oldModel <> model then
                logger.LogInfo "%O" model
                model
            else
                model

        let logInc (clientMsg: RemoteServerMsg) =
            match clientMsg with
            | BridgeOps(Misc(IsMordhauRunning), Caller.App) ->
                logger.LogVerbose "New server message:\n%O" clientMsg
            | _ ->
                logger.LogInfo "New server message:\n%O" clientMsg

        let private init (clientDispatch: Dispatch<RemoteClientMsg>) () =
            Connected |> cDispatchWithLog clientDispatch
            { Game = None
              GameUserSettings = None
              Engine = None
              BackupSettings = KeepAll
              UpdatePending = None }
            |> fun iModel ->
                logger.LogInfo "Init model:\n%O" iModel
                iModel
            |> fun m -> m, Cmd.none

        let private createClientResp (caller: Caller) (file: INIFile option) (br: BridgeResult) =
            { Caller = caller
              File = file
              BridgeResult = br }
            |> Some

        let private cleanBackupsAsync (bSet: BackupSettings) (fList: INIFile list) =
            async {
                do! Async.Sleep 10000
                fList |> List.iter (BridgeOperations.INI.cleanBackups bSet)
            }
            |> Async.Start

        let private updateModel (file: ConfigFile) (iOpt: INIValue option) model =
            match file with
            | ConfigFile.Game -> { model with Game = iOpt }
            | ConfigFile.GameUserSettings -> { model with GameUserSettings = iOpt }
            | ConfigFile.Engine -> { model with Engine = iOpt }

        let update (clientDispatch: Dispatch<RemoteClientMsg>) (ClientMsg clientMsg) (model: Model) =
            logInc clientMsg

            let m, remoteCMsg =
                match clientMsg with
                | BridgeOps(ops, caller) ->
                    match ops with
                    | CommunityOperation cCmd ->
                        match cCmd with
                        | GetSteamAnnouncements ->
                            model,
                            Community.getSteamAnn()
                            |> CommunityResult.SteamAnnouncements
                            |> BridgeResult.Community
                            |> createClientResp caller None
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
                        | INIFileOperation.Exists iFile ->
                            model,
                            INI.exists iFile
                            |> INIOperationResult.Exists
                            |> BridgeResult.INIOperation
                            |> createClientResp caller (Some(iFile))
                        | INIFileOperation.Parse iFile ->
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
                                |> cDispatchWithLog clientDispatch
                            | ConfigFile.Engine when model.GameUserSettings.IsSome ->
                                { cmd.Value with Caller = Caller.MordhauConfig }
                                |> Resp
                                |> cDispatchWithLog clientDispatch
                            | ConfigFile.GameUserSettings when model.Engine.IsSome ->
                                { cmd.Value with Caller = Caller.MordhauConfig }
                                |> Resp
                                |> cDispatchWithLog clientDispatch
                            | _ -> ()
                            m, cmd
                        | INIFileOperation.Backup fList ->
                            cleanBackupsAsync model.BackupSettings fList
                            model,
                            fList
                            |> List.map INI.backup
                            |> List.forall id
                            |> INIOperationResult.Backup
                            |> BridgeResult.INIOperation
                            |> createClientResp caller None
                        | INIFileOperation.Commit fList ->
                            model,
                            fList
                            |> List.map (fun iFile -> INI.write iFile (model.GetIVal(iFile.File).Value))
                            |> List.forall id
                            |> INIOperationResult.CommitChanges
                            |> BridgeResult.INIOperation
                            |> createClientResp caller None
                    | Faces fCmd ->
                        let cResp br = createClientResp caller None br
                        match fCmd with
                        | Random profiles ->
                            let result = INI.random profiles (model.GetIVal(ConfigFile.Game).Value)
                            updateModel ConfigFile.Game result model, result.IsSome |> FaceResult.Random
                        | Frankenstein profiles ->
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
                        | GetConfigs oList ->
                            model,
                            oList
                            |> INI.getConfigs engine gameUser
                            |> ConfigResult.GetConfigs
                        | MapConfigs oList ->
                            let newEngine, newGameUser = oList |> INI.mapConfigs engine gameUser
                            model
                            |> (updateModel ConfigFile.Engine newEngine >> updateModel ConfigFile.Game newGameUser),
                            (newEngine.IsSome && newGameUser.IsSome) |> ConfigResult.MapConfigs
                        |> fun (m, cr) ->
                            m,
                            cr
                            |> BridgeResult.Config
                            |> cResp
                    | SettingsOperation sCmd ->
                        let cResp br = createClientResp caller None br
                        match sCmd with
                        | EnableAutoLaunch ->
                            model,
                            Settings.enableAutoLaunch()
                            |> SettingResult.EnabledAutoLaunch
                            |> BridgeResult.Settings
                            |> cResp
                        | DisableAutoLaunch ->
                            model,
                            Settings.disableAutoLaunch()
                            |> SettingResult.DisabledAutoLaunch
                            |> BridgeResult.Settings
                            |> cResp
                        | BackupPolicy bSet -> { model with BackupSettings = bSet }, None
                        | SetupLinux ->
                            Settings.setupLinux()
                            model, None
                    | Updates uCmd ->
                        let cResp br = createClientResp caller None br
                        match uCmd with
                        | Updates.Start ->
                            match model.UpdatePending with
                            | Some file -> Updating.installUpdates file
                            | _ -> Result.Error "No pending file"
                            |> function
                            | Ok(Some file) ->
                                Updating.cleanUpdatingDir()
                                Updating.launchNewVersion (file)
                                { model with UpdatePending = None },
                                UpdateResult.Complete
                                |> BridgeResult.Updates
                                |> cResp
                            | Ok _ ->
                                Updating.cleanUpdatingDir()
                                { model with UpdatePending = None },
                                UpdateResult.Failed
                                |> BridgeResult.Updates
                                |> cResp
                            | Result.Error e ->
#if DEBUG
                                System.Console.WriteLine e
#endif
                                model, None
                        | Updates.Check ->
                            match Updating.getUpdates() with
                            | Ok res ->
                                { model with UpdatePending = res },
                                match res with
                                | Some _ ->
                                    UpdateResult.Ready
                                    |> BridgeResult.Updates
                                    |> cResp
                                | _ -> None
                            | Result.Error e ->
#if DEBUG
                                System.Console.WriteLine e
#endif
                                model, None
                    | Misc mCmd ->
                        let cResp br = createClientResp caller None br
                        match mCmd with
                        | MiscOperation.IsMordhauRunning ->
                            model,
                            Misc.isMordhauRunning()
                            |> MiscResult.MordhauRunning
                            |> BridgeResult.Misc
                            |> cResp

            match remoteCMsg with
            | Some rMsg -> Resp rMsg |> cDispatchWithLog clientDispatch
            | _ -> ()

            modelUpdateWithLog model m, Cmd.none

        let bridge = Bridge.mkServer BridgeOperations.Endpoint init update |> Bridge.run Giraffe.server

    let server = router { get BridgeOperations.Endpoint bridge }

    let app =
        application {
            use_router server
            disable_diagnostics
            app_config Giraffe.useWebSockets
            url (sprintf "http://0.0.0.0:%i/" port)
        }

    [<EntryPoint>]
    let main _ =
        cleanUpLogging (logPath)
        logger.LogInfo "Working directory - %s" <| System.IO.Directory.GetCurrentDirectory()

        try
            run app
        with e ->
            logger.LogFatal "Fatal exception occured:\n%O" e
            reraise()

        0 // return an integer exit code
