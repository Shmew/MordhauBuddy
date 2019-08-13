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
        | EngineTools -> "Custom settings"

    let window = getRemoteWin()

    let init() =
        let m =
            { Page = Home
              IsMax = window.isMaximized() 
              IsDarkTheme = true //use store to get this later
              IsBridgeConnected = false
              ContextMenu = ContextMenu.State.init()
              FaceTools = FaceTools.State.init()
              EngineTools = EngineTools.State.init() }
        m, Cmd.none

    let update msg m =
        match msg with
        | Navigate msg' ->
            { m with Page = msg' }, Cmd.none
        | MinMaxMsg msg' ->
            { m with IsMax = msg' }, Cmd.none
        | DarkThemeMsg msg' ->
            { m with IsDarkTheme = msg' }, Cmd.none
        | ContextMenuMsg msg' ->
            let m',cmd = ContextMenu.State.update msg' m.ContextMenu
            { m with ContextMenu = m' }, Cmd.map ContextMenuMsg cmd
        | FaceToolsMsg msg' ->
            let m', cmd = FaceTools.State.update msg' m.FaceTools
            { m with FaceTools = m' }, Cmd.map FaceToolsMsg cmd
        | EngineToolsMsg msg' ->
            let m', cmd = EngineTools.State.update msg' m.EngineTools
            { m with EngineTools = m' }, Cmd.map EngineToolsMsg cmd
        | ServerMsg msg' ->
            match msg' with
            | Resp bRes ->
                let m', cmd = FaceTools.State.update (FaceTools.Types.ClientMsg bRes) m.FaceTools
                { m with FaceTools = m' }, Cmd.map FaceToolsMsg cmd
            | Connected ->
                { m with IsBridgeConnected = true}, Cmd.none
            | Disconnected ->
                { m with IsBridgeConnected = false}, Cmd.none

