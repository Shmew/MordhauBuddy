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
            |> List.exists (fun m -> 
                m.Map.Name = map.Map.Name && m.Map.Version = map.Map.Version)
            |> not)

    let private partitionComMaps (cList : CommunityMapWithProgress list) map =
        cList |> List.partition (fun m -> m.Map.GetName() = map)

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
                | MapOperationResult.Delete (map, res) ->
                    match res with
                    | Ok(_) ->
                        { model with 
                            Installed = (model.Installed |> List.filter (fun m -> m.Map.Folder <> map)) }
                        , Cmd.ofMsg GetAvailable
                    | Error(e) ->
                        { model with 
                            Installed = 
                                (model.Installed 
                                 |> List.map (fun m -> 
                                    if m.Map.Folder = map then 
                                        { m with Error = true; HelperText = e } 
                                    else m)) }, Cmd.none
                | _ -> model, Cmd.none
            | BridgeResult.Maps mRes ->
                match mRes with
                | MapResult.AvailableMaps cList ->
                    { model with Available = cList |> List.map (getComMap >> CommunityMapWithProgress.Init) }
                    |> fun newM -> { newM with Available = calcAvailableMaps newM }, Cmd.none
                | MapResult.InstalledMaps cList ->
                    { model with Installed = (cList |> List.map (getComMap >> CommunityMapWithProgress.Init)) }
                    |> fun newM -> { newM with Available = calcAvailableMaps newM }, Cmd.none
                | MapResult.InstallMap (map, res) ->
                    match res with
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
                    | _ -> model, Cmd.none
                | MapResult.InstallMapCancelled (map, b) -> 
                    if b then
                        { model with
                            Installing = (model.Installing |> List.filter (fun m -> m.Map.Folder <> map))}
                        , Cmd.ofMsg GetAvailable
                    else
                        { model with
                            Installing = 
                                (model.Installing 
                                |> List.map (fun m -> 
                                    if m.Map.Folder = map then 
                                        { m with 
                                            Error = true
                                            HelperText = "Error cancelling installation" }
                                    else m))}
                        , Cmd.none
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
                        Installed = finished |> List.append model.Installed
                        Installing = inProcess }, Cmd.bridgeSend (sender.ConfirmInstall map)
                | MapResult.InstallMapError (map, err) -> 
                    { model with
                        Installing = 
                            (model.Installing 
                            |> List.map (fun m -> 
                                if m.Map.Folder = map then 
                                    { m with 
                                        Error = true
                                        HelperText = err } 
                                else m))}
                    , Cmd.none
            | _ -> { model with Waiting = false }, Cmd.none
        | TabSelected tab -> 
            { model with TabSelected = tab }, Cmd.none
        | ImgSkeleton -> model, Cmd.none
        | Install (name,fName) -> 
            let newInstalling,newAvailable =
                partitionComMaps model.Available name
            let mCmd =
                { MapTarget.Folder = fName
                  MapTarget.Directory = model.MapsDir.Directory
                  MapTarget.GDrive = 
                    let map = newInstalling.Head.Map
                    match map.GoogleDriveID, map.FileSize with
                    | Some(gId), Some(size) ->
                        { GoogleDrive.ID = gId; GoogleDrive.Size = size }
                        |> Some
                    | _ -> None }
            { model with
                Available = newAvailable
                Installing = (List.append model.Installing newInstalling) }
            , Cmd.bridgeSend (sender.Install(mCmd))
        | InstallAll -> 
            model, Cmd.batch 
                (model.Available 
                |> List.map (fun m -> 
                    Install(m.Map.GetName(),m.Map.Folder) |> Cmd.ofMsg))
        | Uninstall s -> model, Cmd.bridgeSend (sender.Uninstall model.MapsDir.Directory s)
        | UninstallAll -> 
            model, Cmd.batch 
                (model.Installed 
                |> List.map (fun m -> 
                    Uninstall(m.Map.Folder) |> Cmd.ofMsg))
        | CancelInstall s -> model, Cmd.bridgeSend (sender.Cancel s)
        | CancelInstallAll -> 
            model, Cmd.batch 
                (model.Installing 
                |> List.map (fun m -> 
                    CancelInstall(m.Map.Folder) |> Cmd.ofMsg))
        | GetInstalled -> model, Cmd.bridgeSend (sender.GetInstalled(model.MapsDir.Directory))
        | GetAvailable -> model, Cmd.bridgeSend (sender.GetAvailable)
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


