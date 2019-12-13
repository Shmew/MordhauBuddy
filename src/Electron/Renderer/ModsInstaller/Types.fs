namespace MordhauBuddy.App.ModsInstaller

module rec Types =
    open FSharp.Core /// To avoid shadowing Result<_,_>
    open MordhauBuddy.App
    open BridgeUtils
    open RenderUtils
    open RenderUtils.Directory
    open MordhauBuddy.Shared.ElectronBridge
    open Microsoft.FSharp.Reflection

    type Tab =
        | Available
        | Installed
        | Installing
        member this.Text = this.ToString() |> String.duToTitle

        static member private Cases = FSharpType.GetUnionCases typeof<Tab>

        static member private Instantiate name =
            Tab.Cases
            |> Array.tryFind (fun uc -> uc.Name = name)
            |> Option.map (fun uc -> Reflection.FSharpValue.MakeUnion(uc, [||]) :?> Tab)
            |> Option.get

        static member GetTabs = Tab.Cases |> Array.map (fun uc -> uc.Name |> Tab.Instantiate)

        member this.GetTag =
            Tab.Cases
            |> Seq.tryFind (fun uc -> uc.Name = this.ToString())
            |> Option.map (fun uc -> uc.Tag)
            |> Option.get

        static member GetTabFromTag(tag: int) =
            Tab.Cases
            |> Seq.tryFind (fun t -> t.Tag = tag)
            |> Option.map (fun uc -> uc.Name |> Tab.Instantiate)
            |> Option.get

    type Msg =
        | ClientMsg of BridgeMsg
        | TabSelected of Tab
        | ImgSkeleton
        | Install of int
        | InstallAll
        | Update of UpdateSettings
        | UpdateMods
        | Uninstall of int
        | Disable of int
        | Enable of int
        | UninstallAll
        | CancelInstall of int
        | CancelInstallAll
        | GetInstalled
        | GetAvailable
        | ToggleMenu of Tab * int

    type MenuPosition =
        { X: int
          Y: int }

    [<RequireQualifiedAccess>]
    type MenuState =
        | Open of MenuPosition
        | Closed
        static member Toggle(mod': ModWithState) =
            match mod'.MenuState with
            | MenuState.Open _ -> MenuState.Closed
            | MenuState.Closed ->
                let pos = getMousePositions()
                MenuState.Open
                    ({ X = pos.X
                       Y = pos.Y })

    [<RequireQualifiedAccess>]
    type ModState =
        | Success of int
        | Error of string

        member this.IsStateSuccess =
            match this with
            | ModState.Success _ -> true
            | ModState.Error _ -> false

        member this.IsStateError =
            match this with
            | ModState.Error _ -> true
            | ModState.Success _ -> false

    type ModWithState =
        { Mod: ModInfoFile
          State: ModState
          MenuState: MenuState
          Disabled: bool }
        static member Init(mod': ModInfoFile) =
            { Mod = mod'
              State = ModState.Success 0
              MenuState = MenuState.Closed
              Disabled = false }

    type Model =
        { ModsDir: ConfigDir
          UpdateSettings: UpdateSettings
          Available: ModWithState list
          Installed: ModWithState list
          Installing: ModWithState list
          UpdatesAvailable: int
          Uninstalling: int list
          Disabling: int list
          Enabling: int list
          ActiveInstalling: int list
          ActiveUninstalling: int option
          ActiveDisabling: int option
          ActiveEnabling: int option
          TabSelected: Tab
          Refreshing: bool
          Updating: bool }