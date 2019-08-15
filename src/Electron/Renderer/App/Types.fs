namespace MordhauBuddy.App

module Types =
    open MordhauBuddy.Shared.ElectronBridge
    open FSharp.Core /// To avoid shadowing Result<_,_>

    type Page =
        | Home
        | FaceTools
        | MordhauConfig
        | Settings
        | About
        static member All =
            [ Home; FaceTools; MordhauConfig; Settings; About ]

    type Msg =
        | Navigate of Page
        | MinMaxMsg of bool
        | StoreMsg of Store.Msg
        | ContextMenuMsg of ContextMenu.Types.Msg
        | FaceToolsMsg of FaceTools.Types.Msg
        | MordhauConfigMsg of MordhauConfig.Types.Msg
        | SettingsMsg of Settings.Types.Msg
        | AboutMsg of About.Types.Msg
        | ServerMsg of RemoteClientMsg

    type Model =
        { Page : Page
          IsMax : bool
          Store : Store.Model
          IsBridgeConnected : bool
          ContextMenu : ContextMenu.Types.Model
          FaceTools : FaceTools.Types.Model
          MordhauConfig : MordhauConfig.Types.Model
          Settings : Settings.Types.Model
          About : About.Types.Model }

