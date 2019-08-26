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
        { MapsDir = 
              { Dir = DirLoad.MapDir
                Directory = ""
                Label = ""
                State = DirState.Init "" }
          Available = []
          Installed = []
          Installing = []
          TabSelected = Available
          Refreshing = false }

    let private sender = new MapBridgeSender(Caller.MapInstaller)

    let private calcAvailableMaps (model : Model) =
        model.Available 
        |> List.filter (fun map -> 
            (List.append model.Installed model.Installing) 
            |> List.exists (fun m -> 
                m.Map.Name = map.Map.Name && m.Map.Version = map.Map.Version)
            |> not)

    let private partitionComMaps (cList : CommunityMapWithState list) map =
        cList |> List.partition (fun m -> m.Map.GetName() = map)

    let private autoRefresh (availMsg : Msg) (installedMsg : Msg) dispatch =
        async {
            while true do
                do! Async.Sleep 1800000
                dispatch availMsg
                do! Async.Sleep 600000
                dispatch installedMsg
        } |> Async.StartImmediate

    let update (msg: Msg) (model: Model) =
        match msg with
        | ClientMsg bMsg ->
            match bMsg.BridgeResult with
            | BridgeResult.MapOperation mOp ->
                match mOp with
                | MapOperationResult.DirExists b ->
                    if b then
                        { model with
                            MapsDir =
                                { model.MapsDir with
                                    State = DirState.Success "Maps directory located" } 
                            Refreshing = true }
                        , Cmd.batch 
                            [ yield Cmd.ofMsg GetAvailable
                              if model.Refreshing |> not then 
                                yield Cmd.ofSub <| autoRefresh GetAvailable GetInstalled ]
                    else
                        { model with
                            MapsDir =
                                { model.MapsDir with
                                    State = DirState.Error "Maps directory not found" } }
                        , Cmd.none
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
                                        { m with State = ComMapState.Error e } 
                                    else m)) }, Cmd.none
                | _ -> model, Cmd.none
            | BridgeResult.Maps mRes ->
                match mRes with
                | MapResult.AvailableMaps cList ->
                    { model with 
                        Available = 
                            cList 
                            |> List.map (getComMap >> CommunityMapWithState.Init) 
                            |> List.sortBy (fun k -> k.Map.GetName()) }
                    |> fun newM -> { newM with Available = calcAvailableMaps newM }, Cmd.none
                | MapResult.InstalledMaps cList ->
                    { model with Installed = (cList |> List.map (getComMap >> CommunityMapWithState.Init)) }
                    |> fun newM -> { newM with Available = calcAvailableMaps newM }, Cmd.none
                | MapResult.InstallMap (map, res) ->
                    match res with
                    | Error(errMsg) ->
                        let setError =
                            model.Installing 
                            |> List.map (fun m -> 
                                if m.Map.Folder = map then 
                                    { m with 
                                        State = ComMapState.Error errMsg }
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
                                            State = ComMapState.Error "Error cancelling installation" }
                                    else m))}
                        , Cmd.none
                | MapResult.InstallMapProgress (map, prog) -> 
                    let newProg = 
                        model.Installing 
                        |> List.map (fun m -> 
                            if m.Map.Folder = map && m.State.IsStateSuccess then 
                                { m with State = ComMapState.Success prog } 
                            else m)
                    { model with Installing = newProg }, Cmd.none
                | MapResult.InstallMapComplete map -> 
                    let finished,inProcess = model.Installing |> List.partition (fun m -> m.Map.Folder = map)
                    { model with 
                        Installed = finished |> List.append model.Installed |> List.sortBy (fun k -> k.Map.GetName())
                        Installing = inProcess }, Cmd.bridgeSend (sender.ConfirmInstall map)
                | MapResult.InstallMapError (map, err) -> 
                    { model with
                        Installing = 
                            (model.Installing 
                            |> List.map (fun m -> 
                                if m.Map.Folder = map then 
                                    { m with 
                                        State = ComMapState.Error err } 
                                else m))}
                    , Cmd.none
            | _ -> model, Cmd.none
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
                Installed = (model.Installed |> List.filter (fun m -> m.Map.Folder <> fName))
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
        | CancelInstall s -> 
            let cancelled,inProcess = model.Installing |> List.partition (fun m -> m.Map.Folder = s)
            { model with 
                Available = cancelled |> List.map (fun m -> m.Map |> CommunityMapWithState.Init) |> List.append model.Available
                Installing = inProcess }, Cmd.bridgeSend (sender.Cancel s)
        | CancelInstallAll -> 
            model, Cmd.batch 
                (model.Installing 
                |> List.map (fun m -> 
                    CancelInstall(m.Map.Folder) |> Cmd.ofMsg))
        | GetInstalled -> model, Cmd.bridgeSend (sender.GetInstalled(model.MapsDir.Directory))
        | GetAvailable -> model, Cmd.bridgeSend (sender.GetAvailable)
        | ToggleMenu (tab, fName) -> 
            match tab with
            | Available -> model, Cmd.none
            | Installed -> 
                { model with
                    Installed = 
                        (model.Installed 
                         |> List.map (fun m -> 
                            if m.Map.Folder = fName then 
                                { m with 
                                    MenuState = MenuState.Toggle(m) } 
                            else m ))}, Cmd.none
            | Installing ->
                { model with
                    Installing = 
                        (model.Installing 
                         |> List.map (fun m -> 
                            if m.Map.Folder = fName then 
                                { m with 
                                    MenuState = MenuState.Toggle(m) } 
                            else m ))}, Cmd.none
