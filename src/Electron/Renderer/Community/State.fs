namespace MordhauBuddy.App.Community

module State =
    open FSharp.Core // To avoid shadowing Result<_,_>
    open MordhauBuddy.App
    open RenderUtils
    open RenderUtils.Validation
    open Elmish
    open Elmish.Bridge
    open MordhauBuddy.Shared.ElectronBridge
    open BridgeUtils
    open RenderUtils.HtmlParsing
    open Types

    let init() =
        { TabSelected = Announcements
          Refreshing = false
          LoadingElem = true
          SteamAnnouncements = [] }

    [<AutoOpen>]
    module private Helpers =
        let sender = new CommunityBridgeSender(Caller.Community)

        let autoRefresh dispatch =
            async {
                while true do
                    do! Async.Sleep 1800000
                    dispatch Refresh
            }
            |> Async.StartImmediate

    let update (msg: Msg) (model: Model) =
        match msg with
        | ClientMsg bMsg ->
            match bMsg.BridgeResult with
            | BridgeResult.Community cMsg ->
                match cMsg with
                | CommunityResult.SteamAnnouncements sList ->
                    { model with SteamAnnouncements = sList |> formatRawHtml "steamAnnouncements" },
                    Cmd.ofMsg StartRefresh
            | _ -> model, Cmd.none
        | TabSelected tab -> { model with TabSelected = tab }, Cmd.ofMsg Refresh
        | StartRefresh ->
            if model.Refreshing
            then model, Cmd.none
            else { model with Refreshing = true }, Cmd.ofSub autoRefresh
        | Refresh -> model, Cmd.bridgeSend (sender.GetSteamAnnouncements())
