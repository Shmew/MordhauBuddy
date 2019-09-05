namespace MordhauBuddy.App.Community

module Types =
    open System
    open Fable.Core
    open Fable.React
    open Fable.React.Props
    open Fable.MaterialUI
    open Fable.MaterialUI.Core
    open FSharp.Core /// To avoid shadowing Result<_,_>
    open MordhauBuddy.App
    open RenderUtils
    open RenderUtils.Directory
    open MordhauBuddy.Shared.ElectronBridge
    open Microsoft.FSharp.Reflection

    type Tab =
        | Announcements
        | Venatus
        | MCL
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
        | StartRefresh
        | Refresh

    type Model =
        { TabSelected: Tab
          Refreshing: bool
          LoadingElem: bool
          SteamAnnouncements: (string * string) list }
