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
        | LoadResources of Msg
        | LoadConfig of ConfigFile
        | LoadMap
        | StoreMsg of Store.Msg
        | ContextMenuMsg of ContextMenu.Types.Msg
        | FaceToolsMsg of FaceTools.Types.Msg
        | MordhauConfigMsg of MordhauConfig.Types.Msg
        | SettingsMsg of Settings.Types.Msg
        | AboutMsg of About.Types.Msg
        | ServerMsg of RemoteClientMsg

    type ConfigDir =
        { Path : string
          Exists : bool
          Parsed : bool
          AttemptedLoad : bool
          Loading : bool }

    type MapDir =
        { Path : string 
          Exists : bool
          AttemptedLoad : bool 
          Loading : bool }

    type Loaded =
        { GameConfig : ConfigDir
          EngineConfig : ConfigDir
          GameUserConfig : ConfigDir
          Maps : MapDir }

    type Model =
        { Page : Page
          IsMax : bool
          Store : Store.Model
          IsBridgeConnected : bool
          Resources : Loaded
          ContextMenu : ContextMenu.Types.Model
          FaceTools : FaceTools.Types.Model
          MordhauConfig : MordhauConfig.Types.Model
          Settings : Settings.Types.Model
          About : About.Types.Model }

