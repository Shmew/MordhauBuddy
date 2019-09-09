namespace MordhauBuddy.App

module Types =
    open MordhauBuddy.Shared.ElectronBridge
    open FSharp.Core /// To avoid shadowing Result<_,_>

    type Page =
        | Community
        | MapsInstaller
        | FaceTools
        | MordhauConfig
        | Settings
        | About
        static member All = [ Community; MapsInstaller; FaceTools; MordhauConfig; Settings; About ]

    type Msg =
        | Navigate of Page
        | MinMaxMsg of bool
        | LoadResources of Msg
        | ResourcesLoaded
        | LoadCom
        | LoadConfig of ConfigFile
        | LoadMap
        | InitSetup
        | StartCheckUpdates
        | CheckUpdates
        | StartPatch
        | StoreMsg of Store.Msg
        | ContextMenuMsg of ContextMenu.Types.Msg
        | CommunityMsg of Community.Types.Msg
        | MapsInstallerMsg of MapsInstaller.Types.Msg
        | FaceToolsMsg of FaceTools.Types.Msg
        | MordhauConfigMsg of MordhauConfig.Types.Msg
        | SettingsMsg of Settings.Types.Msg
        | AboutMsg of About.Types.Msg
        | ServerMsg of RemoteClientMsg

    type ConfigDir =
        { Path: string
          Exists: bool
          Parsed: bool
          AttemptedLoad: bool
          Loading: bool }

    type MapDir =
        { Path: string
          Exists: bool
          AttemptedLoad: bool
          Loading: bool }

    type ComResources =
        { AttemptedLoad: bool }

    type SetupResource =
        { AttemptedLoad: bool }

    type Loaded =
        { InitSetup: SetupResource
          Community: ComResources
          GameConfig: ConfigDir
          EngineConfig: ConfigDir
          GameUserConfig: ConfigDir
          Maps: MapDir }

    type UpdatePending =
        { Refreshing: bool
          Ready: bool
          Error: bool }

    type Model =
        { Page: Page
          IsMax: bool
          Store: Store.Model
          IsBridgeConnected: bool
          UpdatePending: UpdatePending
          Resources: Loaded
          ContextMenu: ContextMenu.Types.Model
          Community: Community.Types.Model
          MapsInstaller: MapsInstaller.Types.Model
          FaceTools: FaceTools.Types.Model
          MordhauConfig: MordhauConfig.Types.Model
          Settings: Settings.Types.Model
          About: About.Types.Model }

    type AppTheme =
        { PaletteType: Fable.MaterialUI.Themes.PaletteType
          PMain: string
          PDark: string
          PCText: string option
          SMain: string
          SDark: string option
          SCText: string option
          EMain: string
          ECText: string option
          PaperElev2: string
          MuiButtonCPHover: string option
          MuiButtonCSecondary: string option }
