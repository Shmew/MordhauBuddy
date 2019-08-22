namespace MordhauBuddy.App.MapsInstaller

module State =
    open FSharp.Core  // To avoid shadowing Result<_,_>
    open MordhauBuddy.App
    open RenderUtils
    open RenderUtils.Validation
    open RenderUtils.WebParsing
    open Elmish
    open Elmish.Bridge
    open MordhauBuddy.Shared.ElectronBridge
    open BridgeUtils
    open RenderUtils.Directory
    open Types
    open Electron

    let init() =
        { Waiting = true
          MapsDir = 
              { Dir = DirLoad.MapDir
                Label = ""
                Waiting = false
                Directory = ""
                Error = false
                HelperText = "" 
                Validated = false }
          Available = []
          Installed = []
          Installing = []
          TabSelected = Available
          Snack = Snackbar.State.init() }

    let private sender = new MapBridgeSender(Caller.MapInstaller)

    let private calcAvailableMaps (model : Model) =
        model.Available 
        |> List.filter (fun map -> 
            (List.append model.Installed model.Installing) 
            |> List.exists (fun (m : MapTypes.CommunityMap) -> 
                m.Name = map.Name && m.Version = map.Version)
            |> not)

    let update (msg: Msg) (model: Model) =
        match msg with
        | ClientMsg bMsg ->
            match bMsg.BridgeResult with
            | BridgeResult.MapOperation mOp ->
                match mOp with
                | MapOperationResult.DirExists b ->
                    if b then
                        { model with
                            Waiting = false 
                            MapsDir =
                                { model.MapsDir with
                                    Waiting = false
                                    Error = false
                                    HelperText = "Maps directory located"
                                    Validated = true } }, Cmd.ofMsg GetAvailable
                    else
                        { model with
                            Waiting = false 
                            MapsDir =
                                { model.MapsDir with
                                    Waiting = false
                                    Error = true
                                    HelperText = "Maps directory not found"
                                    Validated = false } }, Cmd.none
                | _ -> model, Cmd.none
            | BridgeResult.Maps mRes ->
                match mRes with
                | MapResult.AvailableMaps cList ->
                    { model with Available = cList |> List.map getComMap }
                    |> fun newM -> { newM with Available = calcAvailableMaps newM }, Cmd.none
                | MapResult.InstalledMaps cList ->
                    { model with Installed = (cList |> List.map getComMap) }
                    |> fun newM -> { newM with Available = calcAvailableMaps newM }, Cmd.none
            | _ -> { model with Waiting = false }, Cmd.none
        | TabSelected i -> model, Cmd.none
        | ImgSkeleton -> model, Cmd.none
        | Install s -> model, Cmd.none
        | Uninstall s -> model, Cmd.none
        | CancelInstall s -> model, Cmd.none
        | Update s -> model, Cmd.none
        | GetInstalled -> model, Cmd.bridgeSend (sender.GetInstalled(model.MapsDir.Directory))
        | GetAvailable -> model, Cmd.bridgeSend (sender.GetAvailable)
        | Refresh -> model, Cmd.none
        | SnackMsg msg' ->
            let m, cmd, actionCmd = Snackbar.State.update msg' model.Snack
            { model with Snack = m },
            Cmd.batch [ Cmd.map SnackMsg cmd; actionCmd ]
        | SnackDismissMsg ->
            let cmd =
                Snackbar.State.create ""
                |> Snackbar.State.withDismissAction "OK"
                |> Snackbar.State.withTimeout 80000
                |> Snackbar.State.add
            model, Cmd.map SnackMsg cmd


