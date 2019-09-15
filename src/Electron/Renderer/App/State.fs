namespace MordhauBuddy.App

module State =
    open Elmish
    open RenderUtils
    open Elmish.Bridge
    open Types
    open MordhauBuddy.Shared.ElectronBridge

    let pageTitle =
        function
        | Community -> "Community"
        | FaceTools -> "Face Tools"
        | MordhauConfig -> "Mordhau Configuration"
        | Settings -> "Settings"
        | About -> "About"

    let window = getRemoteWin()
    let private store = Store.init()

    let init() =
        let backupSettings =
            BackupSettings.TryGetSettingFromText store.BackupSettings
            |> defaultArg
            <| KeepLast10

        let m =
            { Page = Community
              IsMax = window.isMaximized()
              Store = store
              IsBridgeConnected = false
              MordhauChecking = false
              MordhauRunning = false
              UpdatePending =
                  { Refreshing = false
                    Ready = false
                    Error = false }
              Resources =
                  { InitSetup = { AttemptedLoad = false }
                    Community = { AttemptedLoad = false }
                    GameConfig =
                        { Path = defaultArg store.GameLocation ""
                          Exists = false
                          Parsed = false
                          AttemptedLoad = false
                          Loading = false }
                    EngineConfig =
                        { Path = defaultArg store.EngineLocation ""
                          Exists = false
                          Parsed = false
                          AttemptedLoad = false
                          Loading = false }
                    GameUserConfig =
                        { Path = defaultArg store.GameUserLocation ""
                          Exists = false
                          Parsed = false
                          AttemptedLoad = false
                          Loading = false } }
              ContextMenu = ContextMenu.State.init()
              Community = Community.State.init()
              FaceTools = FaceTools.State.init()
              MordhauConfig = MordhauConfig.State.init()
              Settings = Settings.State.init (backupSettings, store.AutoLaunch)
              About = About.State.init() }
        m, Cmd.none

    [<AutoOpen>]
    module Helpers =

        let inline setPath s (cDir: ConfigDir) = { cDir with Path = s }
        let inline setParsed b (cDir: ConfigDir) = { cDir with Parsed = b }
        let inline setExists b (cDir: ConfigDir) = { cDir with Exists = b }
        let inline setAttemptedLoad b (cDir: ConfigDir) = { cDir with AttemptedLoad = b }
        let inline setLoading b (cDir: ConfigDir) = { cDir with Loading = b }

        let inline setResLoaded (cFile: ConfigFile) (res: Loaded) (cDir: ConfigDir) =
            match cFile with
            | ConfigFile.Game-> { res with GameConfig = cDir }
            | ConfigFile.Engine -> { res with EngineConfig = cDir }
            | ConfigFile.GameUserSettings-> { res with GameUserConfig = cDir }

        let inline setUpdateRefreshing (b: bool) (up: UpdatePending) = { up with Refreshing = b }
        let inline setUpdateReady (b: bool) (up: UpdatePending) = { up with Ready = b }
        let inline setUpdateError (b: bool) (up: UpdatePending) = { up with Error = b }

        let setUpdate (model: Model) (up: UpdatePending) = { model with UpdatePending = up }
        let setResource (model: Model) (res: Loaded) = { model with Resources = res }
        let setSettings (model: Model) (set: Settings.Types.Model) = { model with Settings = set }
        let setFaceTools (model: Model) (fTool: FaceTools.Types.Model) = { model with FaceTools = fTool }
        let setMordConfig (model: Model) (mConf: MordhauConfig.Types.Model) = { model with MordhauConfig = mConf }

        let setMordRunning (model: Model) b =
            let mRunningMC (model: Model) =
                { model.MordhauConfig with
                      Submit =
                          if b then RenderTypes.Submit.Error "Mordhau is running"
                          else RenderTypes.Submit.Init }
                |> setMordConfig model

            let mRunningFT (model: Model) =
                { model.FaceTools with
                      Submit =
                          if b then RenderTypes.Submit.Error "Mordhau is running"
                          else RenderTypes.Submit.Init }
                |> setFaceTools model

            { model with MordhauRunning = b }
            |> mRunningMC
            |> mRunningFT

        let settingsSender = BridgeUtils.SettingBridgeSender(Caller.Settings)
        let comSender = BridgeUtils.CommunityBridgeSender(Caller.Community)
        let appSender = BridgeUtils.AppBridgeSender(Caller.App)

    let private checkForUpdates dispatch =
        async {
            while true do
                dispatch CheckUpdates
                do! Async.Sleep 10800000
        }
        |> Async.StartImmediate

    let private checkForMordhau dispatch =
        async {
            while true do
                dispatch CheckMordhau
                do! Async.Sleep 10000
        }
        |> Async.StartImmediate

    let private mordhauClosed (model: Model) =
        let parseEngine =
            { File = ConfigFile.Engine
              WorkingDir = model.Settings.EngineDir.Directory |> Some }
            |> BridgeUtils.INIBridgeSender(Caller.MordhauConfig).Parse

        let parseGameUser =
            { File = ConfigFile.GameUserSettings
              WorkingDir = model.Settings.GameUserDir.Directory |> Some }
            |> BridgeUtils.INIBridgeSender(Caller.MordhauConfig).Parse

        [ Cmd.bridgeSend parseEngine
          Cmd.bridgeSend parseGameUser ]

    let update msg m =
        match msg with
        | Navigate msg' -> { m with Page = msg' }, Cmd.none
        | MinMaxMsg msg' -> { m with IsMax = msg' }, Cmd.none
        | LoadResources msg' ->
            match msg' with
            | InitSetup ->
                { m.Resources with InitSetup = { m.Resources.InitSetup with AttemptedLoad = true } } |> setResource m,
                Cmd.ofMsg InitSetup
            | LoadCom ->
                { m.Resources with Community = { m.Resources.Community with AttemptedLoad = true } } |> setResource m,
                Cmd.ofMsg LoadCom
            | LoadConfig(cFile) ->
                match cFile with
                | ConfigFile.Game -> setLoading true m.Resources.GameConfig
                | ConfigFile.Engine -> setLoading true m.Resources.EngineConfig
                | ConfigFile.GameUserSettings -> setLoading true m.Resources.GameUserConfig
                |> setResLoaded cFile m.Resources
                |> setResource m
                |> fun m' -> m', Cmd.ofMsg <| LoadConfig(cFile)
            | _ -> m, Cmd.none
        | ResourcesLoaded ->
            { m with Community = { m.Community with LoadingElem = false } },
            Cmd.batch
                [ Cmd.ofMsg StartCheckUpdates
                  Cmd.ofMsg StartCheckMordhau ]
        | LoadCom -> m, Cmd.bridgeSend <| comSender.GetSteamAnnouncements()
        | LoadConfig cFile ->
            let resource, error, setResPath =
                match cFile with
                | ConfigFile.Game ->
                    m.Resources.GameConfig, m.Settings.GameDir.State.IsDirError,
                    (fun (sModel: Settings.Types.Model) -> setPath sModel.GameDir.Directory m.Resources.GameConfig)
                | ConfigFile.Engine ->
                    m.Resources.EngineConfig, m.Settings.EngineDir.State.IsDirError,
                    (fun (sModel: Settings.Types.Model) -> setPath sModel.EngineDir.Directory m.Resources.EngineConfig)
                | ConfigFile.GameUserSettings ->
                    m.Resources.GameUserConfig, m.Settings.GameUserDir.State.IsDirError,
                    (fun (sModel: Settings.Types.Model) ->
                    setPath sModel.GameUserDir.Directory m.Resources.GameUserConfig)

            let loadConfig m (m', cmd) =
                setResPath m'
                |> setAttemptedLoad true
                |> setResLoaded cFile m.Resources
                |> setResource m
                |> setSettings
                <| m'
                |> fun m -> m, Cmd.map SettingsMsg cmd

            match resource.Path with
            | "" when resource.AttemptedLoad |> not ->
                Settings.State.update (Settings.Types.GetDefaultDir) m.Settings |> loadConfig m
            | s when error |> not ->
                Settings.State.update (Settings.Types.SetConfigDir(s, Ok s, cFile)) m.Settings |> loadConfig m
            | _ ->
                resource
                |> setAttemptedLoad true
                |> setResLoaded cFile m.Resources
                |> setResource m, Cmd.none
        | InitSetup ->
            let m', cmd = Settings.State.update (Settings.Types.RunSetup) m.Settings
            { m with Settings = m' }, Cmd.map SettingsMsg cmd
        | StartCheckMordhau ->
            if m.MordhauChecking then m, Cmd.none
            else { m with MordhauChecking = true }, Cmd.ofSub checkForMordhau
        | CheckMordhau -> m, Cmd.bridgeSend (appSender.CheckMordhau)
        | StartCheckUpdates ->
            setUpdateRefreshing true m.UpdatePending 
            |> setUpdate m
            |> fun m' ->
                if m.UpdatePending.Refreshing then m', Cmd.none
                else m', Cmd.ofSub checkForUpdates
        | CheckUpdates -> m, Cmd.bridgeSend (appSender.CheckUpdate)
        | StartPatch -> m, Cmd.bridgeSend (appSender.StartUpdate)
        | StoreMsg msg' ->
            let m', cmd = Store.update msg' m.Store, Cmd.none
            { m with Store = m' }, Cmd.map StoreMsg cmd
        | ContextMenuMsg msg' ->
            let m', cmd = ContextMenu.State.update msg' m.ContextMenu
            { m with ContextMenu = m' }, Cmd.map ContextMenuMsg cmd
        | CommunityMsg msg' ->
            let m', cmd = Community.State.update msg' m.Community
            { m with Community = m' }, Cmd.map CommunityMsg cmd
        | FaceToolsMsg msg' ->
            let m', cmd = FaceTools.State.update msg' m.FaceTools
            { m with FaceTools = m' }, Cmd.map FaceToolsMsg cmd
        | MordhauConfigMsg msg' ->
            let m', cmd = MordhauConfig.State.update msg' m.MordhauConfig
            { m with MordhauConfig = m' }, Cmd.map MordhauConfigMsg cmd
        | SettingsMsg msg' ->
            Settings.State.update msg' m.Settings
            |> fun (newM, cmd) ->
                let appendBackup (cList: Cmd<Msg> list) =
                    if m.Settings.BackupSettings <> newM.BackupSettings then
                        newM.BackupSettings
                        |> Store.Msg.SetBackupSettings
                        |> StoreMsg
                        |> Cmd.ofMsg
                        |> fun newCmd -> newCmd :: cList
                    else
                        cList

                let appendDirs (cList: Cmd<Msg> list) =
                    if m.Settings.GameDir <> newM.GameDir then
                        [ Some(newM.GameDir.Directory |> Store.Msg.SetGameLocation) ]
                    else [ None ]
                    |> fun dList ->
                        if m.Settings.EngineDir <> newM.EngineDir then
                            Some(newM.EngineDir.Directory |> Store.Msg.SetEngineLocation) :: dList
                        else dList
                    |> fun dList ->
                        if m.Settings.GameUserDir <> newM.GameUserDir then
                            Some(newM.GameUserDir.Directory |> Store.Msg.SetGameUserLocation) :: dList
                        else dList
                    |> List.choose id
                    |> List.map (fun msg' ->
                        msg'
                        |> StoreMsg
                        |> Cmd.ofMsg)
                    |> List.append cList

                [ Cmd.map SettingsMsg cmd ]
                |> appendBackup
                |> appendDirs
                |> Cmd.batch
                |> fun cmd' -> setSettings m newM, cmd'
        | AboutMsg msg' ->
            let m', cmd = About.State.update msg' m.About
            { m with About = m' }, Cmd.map AboutMsg cmd
        | ServerMsg msg' ->
            let setResource' bMsg (cFile: ConfigFile) =
                let br = bMsg.BridgeResult

                let setResource (model: Model) (cDir: ConfigDir) =
                    cDir
                    |> fun res ->
                        match br with
                        | BridgeResult.INIOperation iOp ->
                            match iOp with
                            | INIOperationResult.DefaultDir(dOpt) ->
                                match dOpt with
                                | Some(p) -> setPath p res
                                | _ -> setPath "" res |> setLoading false
                            | INIOperationResult.Exists b -> setExists b res |> setLoading b
                            | INIOperationResult.Parse b -> setParsed b res |> setLoading false
                            | _ -> res
                        | _ -> res
                        |> setResLoaded cFile model.Resources
                        |> setResource model

                match cFile with
                | ConfigFile.Game ->
                    (fun (model: Model) -> model.Resources.GameConfig |> setResource model)
                    >> (fun model ->
                    { model.FaceTools with
                          GameDir = { model.FaceTools.GameDir with Directory = model.Resources.GameConfig.Path } }
                    |> setFaceTools model)
                | ConfigFile.Engine ->
                    (fun (model: Model) -> model.Resources.EngineConfig |> setResource model)
                    >> (fun model ->
                    { model.MordhauConfig with
                          EngineDir =
                              { model.MordhauConfig.EngineDir with Directory = model.Resources.EngineConfig.Path } }
                    |> setMordConfig model)
                | ConfigFile.GameUserSettings ->
                    (fun (model: Model) -> model.Resources.GameUserConfig |> setResource model)
                    >> (fun model ->
                    { model.MordhauConfig with
                          GameUserDir =
                              { model.MordhauConfig.GameUserDir with Directory = model.Resources.GameUserConfig.Path } }
                    |> setMordConfig model)
            match msg' with
            | Resp(bMsg) ->
                match bMsg.Caller with
                | Caller.Community ->
                    let m', cmd = Community.State.update (Community.Types.ClientMsg bMsg) m.Community
                    { m with Community = m' }, Cmd.map CommunityMsg cmd
                | Caller.FaceTools ->
                    let m', cmd = FaceTools.State.update (FaceTools.Types.ClientMsg bMsg.BridgeResult) m.FaceTools
                    { m with FaceTools = m' }, Cmd.map FaceToolsMsg cmd
                | Caller.MordhauConfig ->
                    let m', cmd = MordhauConfig.State.update (MordhauConfig.Types.ClientMsg bMsg) m.MordhauConfig
                    { m with MordhauConfig = m' }, Cmd.map MordhauConfigMsg cmd
                | Caller.Settings ->
                    let m', cmd = Settings.State.update (Settings.Types.ClientMsg bMsg) m.Settings
                    match bMsg.File, bMsg.BridgeResult with
                    | Some(iFile), _ -> { m with Settings = m' } |> setResource' bMsg (iFile.File), []
                    | None, BridgeResult.INIOperation(INIOperationResult.DefaultDir _) ->
                        [ (m.Resources.GameConfig.Loading, ConfigFile.Game)
                          (m.Resources.EngineConfig.Loading, ConfigFile.Engine)
                          (m.Resources.GameUserConfig.Loading, ConfigFile.GameUserSettings) ]
                        |> List.tryFind (fst)
                        |> function
                        | Some(res) -> setSettings m m' |> setResource' bMsg (res |> snd), []
                        | _ -> setSettings m m', []
                    | None, BridgeResult.Settings(SettingResult.DisabledAutoLaunch true) ->
                        setSettings m m', [ Cmd.ofMsg (StoreMsg(Store.Msg.AutoLaunchSet false)); Cmd.ofMsg (StoreMsg(Store.Msg.AutoLaunch false)) ]
                    | None, BridgeResult.Settings(SettingResult.EnabledAutoLaunch true) ->
                        setSettings m m', [ Cmd.ofMsg (StoreMsg(Store.Msg.AutoLaunchSet true)); Cmd.ofMsg (StoreMsg(Store.Msg.AutoLaunch true)) ]
                    | _ -> setSettings m m', []
                    |> fun (newM, cmds) -> newM, Cmd.batch ([ Cmd.map SettingsMsg cmd ] |> List.append cmds)
                | Caller.App ->
                    match bMsg.BridgeResult with
                    | BridgeResult.Updates uMsg ->
                        match uMsg with
                        | UpdateResult.Ready -> setUpdateReady true m.UpdatePending |> setUpdate m, Cmd.none
                        | UpdateResult.Complete ->
                            quitApplication()
                            setUpdateReady false m.UpdatePending
                            |> setUpdateError false
                            |> setUpdate m, Cmd.none
                        | UpdateResult.Failed ->
                            setUpdateReady false m.UpdatePending
                            |> setUpdateError true
                            |> setUpdate m, Cmd.none
                    | BridgeResult.Misc mMsg ->
                        match mMsg with
                        | MiscResult.MordhauRunning b ->
                            setMordRunning m b,
                            match b with
                            | false when m.MordhauRunning ->
                                mordhauClosed m
                                |> List.append [ Cmd.ofMsg <| FaceToolsMsg(FaceTools.Types.Msg.StepperRestart) ]
                                |> Cmd.batch
                            | _ -> Cmd.none
                    | _ -> m, Cmd.none
            | Connected ->
                if m.Store.AutoLaunch && (m.Store.AutoLaunchSet |> not) then
                    Cmd.bridgeSend <| settingsSender.EnableAutoLaunch
                else Cmd.none
                |> fun cmd -> { m with IsBridgeConnected = true }, cmd
            | Disconnected -> { m with IsBridgeConnected = false }, Cmd.none
