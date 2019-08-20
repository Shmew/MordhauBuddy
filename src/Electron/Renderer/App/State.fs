namespace MordhauBuddy.App

module State =
    open Elmish
    open RenderUtils
    open Elmish.Bridge
    open Types
    open MordhauBuddy.Shared.ElectronBridge

    let pageTitle =
        function
        | Home -> "Home"
        | MapInstaller -> "Map Installer"
        | FaceTools -> "Face Tools"
        | MordhauConfig -> "Mordhau Configuration"
        | Settings -> "Settings"
        | About -> "About"

    let window = getRemoteWin()

    let init() =
        let store = Store.init()
        let m =
            { Page = Home
              IsMax = window.isMaximized() 
              Store = store
              IsBridgeConnected = false
              Resources =
                { GameConfig =
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
                      Loading = false } 
                  Maps =
                    { Path = defaultArg store.MapsLocation "" 
                      Exists = false 
                      AttemptedLoad = false 
                      Loading = false } }
              ContextMenu = ContextMenu.State.init()
              MapsInstaller = Maps.State.init()
              FaceTools = FaceTools.State.init()
              MordhauConfig = MordhauConfig.State.init()
              Settings = Settings.State.init() 
              About = About.State.init() }
        m, Cmd.none

    let update msg m =
        match msg with
        | Navigate msg' ->
            { m with Page = msg' }, Cmd.none
        | MinMaxMsg msg' ->
            { m with IsMax = msg' }, Cmd.none
        | LoadResources msg' -> 
            match msg' with
            | LoadConfig(cFile) ->
                match cFile with
                | ConfigFile.Game ->
                    { m with 
                        Resources =
                            { m.Resources with
                                GameConfig =
                                    {m.Resources.GameConfig with 
                                        Loading = true } } }
                | ConfigFile.Engine ->
                    { m with 
                        Resources =
                            { m.Resources with
                                EngineConfig =
                                    {m.Resources.EngineConfig with 
                                        Loading = true } } }
                | ConfigFile.GameUserSettings ->
                    { m with 
                        Resources =
                            { m.Resources with
                                GameUserConfig =
                                    {m.Resources.GameUserConfig with 
                                        Loading = true } } }
                |> fun m' -> m',Cmd.ofMsg <| LoadConfig(cFile)
            | LoadMap ->
                { m with 
                    Resources =
                        { m.Resources with
                            Maps =
                                {m.Resources.Maps with 
                                    Loading = true } } }
                ,Cmd.ofMsg LoadMap
            | _ -> m,Cmd.none
        | LoadConfig cFile ->
            let resource, error, setPath =
                match cFile with
                | ConfigFile.Game ->
                    m.Resources.GameConfig, 
                        m.Settings.GameDir.Error, 
                        (fun (sModel : Settings.Types.Model) ->
                            { m.Resources with 
                                GameConfig = 
                                    { m.Resources.GameConfig with 
                                        Path = sModel.GameDir.Directory
                                        AttemptedLoad = true } } )
                | ConfigFile.Engine ->
                    m.Resources.EngineConfig, 
                        m.Settings.EngineDir.Error, 
                        (fun (sModel : Settings.Types.Model) ->
                            { m.Resources with 
                                EngineConfig = 
                                    { m.Resources.EngineConfig with 
                                        Path = sModel.EngineDir.Directory
                                        AttemptedLoad = true } } )
                | ConfigFile.GameUserSettings ->
                    m.Resources.GameUserConfig, 
                        m.Settings.GameUserDir.Error, 
                        (fun (sModel : Settings.Types.Model) ->
                            { m.Resources with 
                                GameUserConfig = 
                                    { m.Resources.GameUserConfig with 
                                        Path = sModel.GameUserDir.Directory
                                        AttemptedLoad = true } } )
            match resource.Path with
            | "" when resource.AttemptedLoad |> not ->
                let m',cmd = Settings.State.update (Settings.Types.GetDefaultDir) m.Settings
                { m with 
                    Resources =
                        setPath m'
                    Settings = m' }
                , Cmd.map SettingsMsg cmd
            | s when error |> not ->
                let m',cmd = Settings.State.update (Settings.Types.SetConfigDir(s,Ok s, cFile)) m.Settings
                { m with 
                    Resources =
                        setPath m'
                    Settings = m' }
                , Cmd.map SettingsMsg cmd
            | _ -> m, Cmd.none
        | LoadMap ->
            match m.Resources.Maps.Path with
            | "" when m.Resources.Maps.AttemptedLoad |> not ->
                let m',cmd = Settings.State.update (Settings.Types.GetMapDir) m.Settings
                { m with 
                    Resources =
                        { m.Resources with 
                            Maps = 
                                { m.Resources.Maps with 
                                    Path = m'.MapsDir.Directory
                                    AttemptedLoad = true } }
                    Settings = m' }
                , Cmd.map SettingsMsg cmd
            | s when m.Resources.Maps.Exists |> not && m.Settings.MapsDir.Error |> not ->
                let m',cmd = Settings.State.update (Settings.Types.SetMapDir(s,Ok s)) m.Settings
                { m with 
                    Resources =
                        { m.Resources with 
                            Maps = 
                                { m.Resources.Maps with 
                                    Path = m'.MapsDir.Directory 
                                    AttemptedLoad = true } } 
                    Settings = m' }
                , Cmd.map SettingsMsg cmd
            | _ -> m, Cmd.none
        | StoreMsg msg' ->
            let m',cmd = Store.update msg' m.Store, Cmd.none
            { m with Store = m' }, Cmd.map StoreMsg cmd
        | ContextMenuMsg msg' ->
            let m',cmd = ContextMenu.State.update msg' m.ContextMenu
            { m with ContextMenu = m' }, Cmd.map ContextMenuMsg cmd
        | MapInstallerMsg msg' ->
            let m', cmd = Maps.State.update msg' m.MapsInstaller
            { m with MapsInstaller = m' }, Cmd.map MapInstallerMsg cmd
        | FaceToolsMsg msg' ->
            let m', cmd = FaceTools.State.update msg' m.FaceTools
            { m with FaceTools = m' }, Cmd.map FaceToolsMsg cmd
        | MordhauConfigMsg msg' ->
            let m', cmd = MordhauConfig.State.update msg' m.MordhauConfig
            { m with MordhauConfig = m' }, Cmd.map MordhauConfigMsg cmd
        | SettingsMsg msg' ->
            let m', cmd = Settings.State.update msg' m.Settings
            { m with Settings = m' }, Cmd.map SettingsMsg cmd
        | AboutMsg msg' ->
            let m', cmd = About.State.update msg' m.About
            { m with About = m' }, Cmd.map AboutMsg cmd
        | ServerMsg msg' ->
            let setResource bMsg (cFile: ConfigFile) =
                let br = bMsg.BridgeResult
                match cFile with
                | ConfigFile.Game ->
                    (fun (model: Model) -> 
                        { model with
                            Resources =
                                { model.Resources with 
                                    GameConfig =
                                        match br with
                                        | BridgeResult.INIOperation iOp ->
                                            match iOp with
                                            | INIOperationResult.DefaultDir(dOpt) ->
                                                { model.Resources.GameConfig with
                                                    Path = defaultArg dOpt "" }
                                            | INIOperationResult.Exists b ->
                                                { model.Resources.GameConfig with 
                                                    Exists = b 
                                                    Loading =  if b |> not then false else true } 
                                            | INIOperationResult.Parse b ->
                                               { model.Resources.GameConfig with 
                                                    Parsed = b 
                                                    Loading = false }
                                            | _ ->  model.Resources.GameConfig
                                        | _ -> model.Resources.GameConfig } } )
                    >> (fun model -> 
                        { model with 
                            FaceTools =
                                { model.FaceTools with 
                                    GameDir = 
                                        { model.FaceTools.GameDir with
                                            Directory = model.Resources.GameConfig.Path } } } )
                | ConfigFile.Engine ->
                    (fun (model: Model) -> 
                        { model with
                            Resources =
                                { model.Resources with 
                                    EngineConfig =
                                        match br with
                                        | BridgeResult.INIOperation iOp ->
                                            match iOp with
                                            | INIOperationResult.DefaultDir(dOpt) ->
                                                { model.Resources.EngineConfig with
                                                    Path = defaultArg dOpt "" }
                                            | INIOperationResult.Exists b ->
                                                { model.Resources.EngineConfig with 
                                                    Exists = b
                                                    Loading =  if b |> not then false else true } 
                                            | INIOperationResult.Parse b ->
                                               { model.Resources.EngineConfig with 
                                                    Path = bMsg.File.Value.WorkingDir.Value
                                                    Parsed = b 
                                                    Loading = false }
                                            | _ ->  model.Resources.EngineConfig
                                        | _ -> model.Resources.EngineConfig } } )
                        >> (fun model -> 
                            { model with 
                                MordhauConfig =
                                    { model.MordhauConfig with 
                                        EngineDir = 
                                            { model.MordhauConfig.EngineDir with
                                                Directory = model.Resources.EngineConfig.Path } } } )
                | ConfigFile.GameUserSettings ->
                    (fun (model: Model) -> 
                        { model with
                            Resources =
                                { model.Resources with 
                                    GameUserConfig =
                                        match br with
                                        | BridgeResult.INIOperation iOp ->
                                            match iOp with
                                            | INIOperationResult.DefaultDir(dOpt) ->
                                                { model.Resources.GameUserConfig with
                                                    Path = defaultArg dOpt "" }
                                            | INIOperationResult.Exists b ->
                                                { model.Resources.GameUserConfig with 
                                                    Exists = b
                                                    Loading = if b |> not then false else true } 
                                            | INIOperationResult.Parse b ->
                                               { model.Resources.GameUserConfig with 
                                                    Path = bMsg.File.Value.WorkingDir.Value
                                                    Parsed = b 
                                                    Loading = false }
                                            | _ ->  model.Resources.GameUserConfig
                                        | _ -> model.Resources.GameUserConfig } } )
                    >> (fun model -> 
                        { model with 
                            MordhauConfig =
                                { model.MordhauConfig with 
                                    GameUserDir = 
                                        { model.MordhauConfig.GameUserDir with
                                            Directory = model.Resources.GameUserConfig.Path } } } )
            match msg' with
            | Resp (bMsg) ->
                match bMsg.Caller with
                | Caller.FaceTools ->
                    let m', cmd = FaceTools.State.update (FaceTools.Types.ClientMsg bMsg.BridgeResult) m.FaceTools
                    { m with FaceTools = m' }, Cmd.map FaceToolsMsg cmd
                | Caller.MordhauConfig ->
                    let m', cmd = MordhauConfig.State.update (MordhauConfig.Types.ClientMsg bMsg) m.MordhauConfig
                    { m with MordhauConfig = m' }, Cmd.map MordhauConfigMsg cmd
                | Caller.Settings ->
                    let m', cmd = Settings.State.update (Settings.Types.ClientMsg bMsg) m.Settings
                    match bMsg.File, bMsg.BridgeResult with
                    | Some(iFile), _ ->
                        { m with Settings = m' } 
                        |> setResource bMsg (iFile.File)
                    | None, BridgeResult.MapOperation(mResult) ->
                        match mResult with
                        | MapOperationResult.DirExists b ->
                            { m with 
                                Resources =
                                    { m.Resources with
                                        Maps =
                                            { m.Resources.Maps with 
                                                Exists = b
                                                Loading = false } }
                                Settings = m' }
                        | MapOperationResult.DefaultDir _ ->
                            { m with 
                                Resources =
                                    { m.Resources with
                                        Maps =
                                            { m.Resources.Maps with 
                                                Path = m'.MapsDir.Directory } }
                                Settings = m' }
                    | None, BridgeResult.INIOperation(INIOperationResult.DefaultDir _) ->
                        [ (m.Resources.GameConfig.Loading, ConfigFile.Game)
                          (m.Resources.EngineConfig.Loading, ConfigFile.Engine)
                          (m.Resources.GameUserConfig.Loading, ConfigFile.GameUserSettings) ]
                        |> List.tryFind (fst)
                        |> function 
                        | Some(res) ->
                            { m with Settings = m' } 
                            |> setResource bMsg (res |> snd)
                        | _ -> { m with Settings = m' } 
                    | _ -> { m with Settings = m' } 
                    , Cmd.map SettingsMsg cmd
                | _ -> m, Cmd.none
            | Connected ->
                { m with IsBridgeConnected = true}, Cmd.none
            | Disconnected ->
                { m with IsBridgeConnected = false}, Cmd.none

