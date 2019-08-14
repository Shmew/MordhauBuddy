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
        | FaceTools -> "Face tools"
        | EngineTools -> "Mordhau Configuration"
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
              ContextMenu = ContextMenu.State.init()
              FaceTools = FaceTools.State.init(defaultArg store.GameLocation "")
              EngineTools = EngineTools.State.init(defaultArg store.EngineLocation "")
              Settings = Settings.State.init() 
              About = About.State.init() }
        m, Cmd.none

    let update msg m =
        match msg with
        | Navigate msg' ->
            { m with Page = msg' }, Cmd.none
        | MinMaxMsg msg' ->
            { m with IsMax = msg' }, Cmd.none
        | StoreMsg msg' ->
            let m',cmd = Store.update msg' m.Store, Cmd.none
            { m with Store = m' }, Cmd.map StoreMsg cmd
        | ContextMenuMsg msg' ->
            let m',cmd = ContextMenu.State.update msg' m.ContextMenu
            { m with ContextMenu = m' }, Cmd.map ContextMenuMsg cmd
        | FaceToolsMsg msg' ->
            let cmd' = 
                match msg' with
                | FaceTools.Types.StepperSubmit ->
                     Cmd.ofMsg <| StoreMsg(Store.Msg.SetGameLocation(m.FaceTools.ConfigDir.Directory))
                | _ -> Cmd.none
            let m', cmd = FaceTools.State.update msg' m.FaceTools
            { m with FaceTools = m' }, Cmd.batch [ Cmd.map FaceToolsMsg cmd; cmd' ]
        | EngineToolsMsg msg' ->
            let m', cmd = EngineTools.State.update msg' m.EngineTools
            { m with EngineTools = m' }, Cmd.map EngineToolsMsg cmd
        | SettingsMsg msg' ->
            let m', cmd = Settings.State.update msg' m.Settings
            { m with Settings = m' }, Cmd.map SettingsMsg cmd
        | AboutMsg msg' ->
            let m', cmd = About.State.update msg' m.About
            { m with About = m' }, Cmd.map AboutMsg cmd
        | ServerMsg msg' ->
            match msg' with
            | Resp bRes ->
                let m', cmd = FaceTools.State.update (FaceTools.Types.ClientMsg bRes) m.FaceTools
                { m with FaceTools = m' }, Cmd.map FaceToolsMsg cmd
            | Connected ->
                { m with IsBridgeConnected = true}, Cmd.none
            | Disconnected ->
                { m with IsBridgeConnected = false}, Cmd.none

