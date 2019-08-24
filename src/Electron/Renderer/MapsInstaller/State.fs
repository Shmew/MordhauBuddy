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
            (List.append model.Installed (model.Installing |> List.map (fun c -> c.Map))) 
            |> List.exists (fun (m : MapTypes.CommunityMap) -> 
                m.Name = map.Name && m.Version = map.Version)
            |> not)

    let private partitionComMaps (cList : MapTypes.CommunityMap list) map =
        let newInstalling,newAvailable =
            cList |> List.partition (fun m -> m.GetName() = map)
        (newInstalling |> List.map CommunityMapWithProgress.Init),newAvailable

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
                | MapResult.InstallMap (map, res) ->
                    match res with
                    | Ok(_) ->
                        let finished,inProcess = model.Installing |> List.partition (fun m -> m.Map.Folder = map)
                        { model with 
                            Installed = (finished |> List.map (fun m -> m.Map))
                            Installing = inProcess }, Cmd.none
                    | Error(errMsg) ->
                        let setError =
                            model.Installing 
                            |> List.map (fun m -> 
                                if m.Map.Folder = map then 
                                    { m with 
                                        Error = true
                                        HelperText = errMsg }
                                else m)
                        { model with Installing = setError }, Cmd.none
                | MapResult.InstallMapCancelled (map, b) -> model, Cmd.none
                | MapResult.InstallMapProgress (map, prog) -> 
                    let newProg = 
                        model.Installing 
                        |> List.map (fun m -> 
                            if m.Map.Folder = map then 
                                { m with Progress = prog } 
                            else m)
                    { model with Installing = newProg }, Cmd.none
                | MapResult.InstallMapComplete map -> 
                    let finished,inProcess = model.Installing |> List.partition (fun m -> m.Map.Folder = map)
                    { model with 
                        Installed = (finished |> List.map (fun m -> m.Map)) |> List.append model.Installed
                        Installing = inProcess }, Cmd.none
                | MapResult.InstallMapError (map, err) -> model, Cmd.none
            | _ -> { model with Waiting = false }, Cmd.none
        | TabSelected tab -> 
            { model with TabSelected = tab }, Cmd.none
        | ImgSkeleton -> model, Cmd.none
        | Install (name,fName) -> 
            let newInstalling,newAvailable =
                partitionComMaps model.Available name
            { model with
                Available = newAvailable
                Installing = (List.append model.Installing newInstalling) }
            , Cmd.bridgeSend (sender.Install(fName,model.MapsDir.Directory))
        | InstallAll -> model, Cmd.none
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


