namespace MordhauBuddy.App.MapsInstaller

module rec Types =
    open FSharp.Core /// To avoid shadowing Result<_,_>
    open MordhauBuddy.App
    open RenderUtils
    open RenderUtils.Directory
    open RenderUtils.WebParsing
    open RenderUtils.MapTypes
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
        | Install of string
        | InstallAll
        | Update of UpdateSettings
        | UpdateMaps
        | Uninstall of string
        | UninstallAll
        | CancelInstall of string
        | CancelInstallAll
        | GetInstalled
        | GetAvailable
        | ToggleMenu of Tab * string

    type MenuPosition =
        { X: int
          Y: int }

    [<RequireQualifiedAccess>]
    type MenuState =
        | Open of MenuPosition
        | Closed
        static member Toggle(map: CommunityMapWithState) =
            match map.MenuState with
            | MenuState.Open _ -> MenuState.Closed
            | MenuState.Closed ->
                let pos = getMousePositions()
                MenuState.Open
                    ({ X = pos.X
                       Y = pos.Y })

    [<RequireQualifiedAccess>]
    type ComMapState =
        | Success of int
        | Error of string

        member this.IsStateSuccess =
            match this with
            | ComMapState.Success _ -> true
            | ComMapState.Error _ -> false

        member this.IsStateError =
            match this with
            | ComMapState.Error _ -> true
            | ComMapState.Success _ -> false

    type CommunityMapWithState =
        { Map: CommunityMap
          State: ComMapState
          MenuState: MenuState }
        static member Init(map: CommunityMap) =
            { Map = map
              State = ComMapState.Success 0
              MenuState = MenuState.Closed }

    type Model =
        { MapsDir: ConfigDir
          UpdateSettings: UpdateSettings
          Available: CommunityMapWithState list
          Installed: CommunityMapWithState list
          Installing: CommunityMapWithState list
          UpdatesAvailable: int
          Uninstalling: string list
          ActiveInstalling: string list
          ActiveUninstalling: string option
          TabSelected: Tab
          Refreshing: bool
          Updating: bool }
