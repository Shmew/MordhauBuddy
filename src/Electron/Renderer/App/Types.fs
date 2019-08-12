namespace MordhauBuddy.App

module Types =
    open MordhauBuddy.Shared.ElectronBridge
    open FSharp.Core /// To avoid shadowing Result<_,_>

    type Page =
        | Home
        | FaceTools
        static member All =
            [ Home; FaceTools ]

    type Msg =
        | Navigate of Page
        | MinMaxMsg of bool
        | DarkThemeMsg of bool
        | ContextMenuMsg of ContextMenu.Types.Msg
        | FaceToolsMsg of FaceTools.Types.Msg
        | ServerMsg of RemoteClientMsg

    type Model =
        { Page : Page
          IsMax : bool
          IsDarkTheme : bool
          IsBridgeConnected : bool
          ContextMenu : ContextMenu.Types.Model
          FaceTools : FaceTools.Types.Model }

