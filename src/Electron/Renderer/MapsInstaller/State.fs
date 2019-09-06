namespace MordhauBuddy.App.MapsInstaller

module State =
    open FSharp.Core // To avoid shadowing Result<_,_>
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

    let init (uSet: UpdateSettings) =
        { MapsDir =
              { Dir = DirLoad.MapDir
                Directory = ""
                Label = ""
                State = DirState.Init "" }
          UpdateSettings = uSet
          Available = []
          Installed = []
          Installing = []
          UpdatesAvailable = 0
          Uninstalling = []
          ActiveInstalling = []
          ActiveUninstalling = None
          TabSelected = Available
          Refreshing = false
          Updating = false }

    [<AutoOpen>]
    module private Helpers =

        let sender = new MapBridgeSender(Caller.MapInstaller)

        let calcAvailableMaps (model: Model) =
            model.Available
            |> List.filter (fun map ->
                (List.append model.Installed model.Installing)
                |> List.exists (fun m -> m.Map.Name = map.Map.Name && m.Map.Version = map.Map.Version)
                |> not)

        let partitionComMaps (cList: CommunityMapWithState list) map =
            cList |> List.partition (fun m -> m.Map.Folder = map)

        let removeFromActive (map: string) (activeList: string list) = activeList |> List.filter (fun f -> f <> map)

        let createMTarget model (cMap: CommunityMapWithState) =
            { MapTarget.Folder = cMap.Map.Folder
              MapTarget.Directory = model.MapsDir.Directory
              MapTarget.GDrive =
                  match cMap.Map.GoogleDriveID, cMap.Map.FileSize with
                  | Some(gId), Some(size) ->
                      { GoogleDrive.ID = gId
                        GoogleDrive.Size = size }
                      |> Some
                  | _ -> None }

        let addNextActive model =
            if model.ActiveInstalling.Length <= 2 then
                model.Installing
                |> List.filter (fun m ->
                    model.ActiveInstalling
                    |> List.contains m.Map.Folder
                    |> not)
                |> List.tryHead
            else None
            |> function
            | Some m ->
                { model with ActiveInstalling = m.Map.Folder :: model.ActiveInstalling },
                Cmd.bridgeSend (sender.Install(createMTarget model m))
            | _ when model.ActiveInstalling.IsEmpty && model.Installing.IsEmpty ->
                model,
                Cmd.batch
                    [ Cmd.ofMsg GetInstalled
                      Cmd.ofMsg GetAvailable ]
            | _ -> model, Cmd.none

        let autoRefresh (availMsg: Msg) (installedMsg: Msg) dispatch =
            async {
                while true do
                    do! Async.Sleep 1800000
                    dispatch installedMsg
                    do! Async.Sleep 600000
                    dispatch availMsg
            }
            |> Async.StartImmediate

        let autoUpdate dispatch =
            async {
                while true do
                    do! Async.Sleep 60000
                    dispatch UpdateMaps
            }
            |> Async.StartImmediate

        let getInstalledAvailable model =
            model.Available
            |> List.choose (fun aMap ->
                if model.Installed |> List.exists (fun iMap -> iMap.Map.Folder = aMap.Map.Folder) then
                    aMap.Map.Folder
                    |> Install
                    |> Cmd.ofMsg
                    |> Some
                else None)

    let update (msg: Msg) (model: Model) =
        match msg with
        | ClientMsg bMsg ->
            match bMsg.BridgeResult with
            | BridgeResult.MapOperation mOp ->
                match mOp with
                | MapOperationResult.DirExists b ->
                    if b then
                        { model with
                              MapsDir = { model.MapsDir with State = DirState.Success "Maps directory located" }
                              Refreshing = true },
                        Cmd.batch
                            [ yield Cmd.ofMsg GetAvailable
                              if model.Refreshing |> not then yield Cmd.ofSub <| autoRefresh GetAvailable GetInstalled ]
                    else
                        { model with MapsDir = { model.MapsDir with State = DirState.Error "Maps directory not found" } },
                        Cmd.none
                | MapOperationResult.Delete(map, res) ->
                    if model.Uninstalling.IsEmpty then { model with ActiveUninstalling = None }
                    else
                        let next = model.Uninstalling.Head
                        { model with
                              Uninstalling = model.Uninstalling.Tail
                              ActiveUninstalling = Some(next) }
                    |> fun m ->
                        match res with
                        | Ok(_) ->
                            { m with Installed = (m.Installed |> List.filter (fun m -> m.Map.Folder <> map)) },
                            Cmd.batch
                                [ yield if m.Uninstalling.IsEmpty
                                           |> not
                                           || m.ActiveUninstalling.IsSome then Cmd.none
                                        else Cmd.ofMsg GetAvailable
                                  match m.ActiveUninstalling with
                                  | Some(s) -> yield Cmd.bridgeSend (sender.Uninstall model.MapsDir.Directory s)
                                  | _ -> () ]
                        | Error(e) ->
                            { m with
                                  Installed =
                                      (m.Installed
                                       |> List.map (fun comMap ->
                                           if comMap.Map.Folder = map then { comMap with State = ComMapState.Error e }
                                           else comMap)) }, Cmd.none
                | _ -> model, Cmd.none
            | BridgeResult.Maps mRes ->
                match mRes with
                | MapResult.AvailableMaps cList ->
                    let m =
                        { model with
                              Available =
                                  cList
                                  |> List.map (getComMap >> CommunityMapWithState.Init)
                                  |> List.sortBy (fun k -> k.Map.GetName()) }
                        |> fun newM ->
                            { newM with
                                  Available = calcAvailableMaps newM
                                  UpdatesAvailable = (getInstalledAvailable model).Length }
                    match model.UpdateSettings with
                    | InstalledAndNew
                    | OnlyInstalled -> Cmd.ofMsg UpdateMaps
                    | _ -> Cmd.none
                    |> fun cmd -> m, cmd
                | MapResult.InstalledMaps cList ->
                    { model with Installed = (cList |> List.map (getComMap >> CommunityMapWithState.Init)) }
                    |> fun newM -> { newM with Available = calcAvailableMaps newM }, Cmd.none
                | MapResult.InstallMap(map, res) ->
                    match res with
                    | Error(errMsg) ->
                        let setError =
                            model.Installing
                            |> List.map (fun m ->
                                if m.Map.Folder = map then { m with State = ComMapState.Error errMsg }
                                else m)
                        { model with Installing = setError }, Cmd.none
                    | _ -> model, Cmd.none
                | MapResult.InstallMapCancelled(map, b) ->
                    if b then
                        { model with
                              Installing = (model.Installing |> List.filter (fun m -> m.Map.Folder <> map))
                              ActiveInstalling = removeFromActive map model.ActiveInstalling }
                        |> addNextActive
                        |> fun (m, cmd) ->
                            m,
                            Cmd.batch
                                [ Cmd.ofMsg GetAvailable
                                  cmd ]
                    else
                        { model with
                              Installing =
                                  (model.Installing
                                   |> List.map (fun m ->
                                       if m.Map.Folder = map then
                                           { m with State = ComMapState.Error "Error cancelling installation" }
                                       else m)) }, Cmd.none
                | MapResult.InstallMapProgress(map, prog) ->
                    let newProg =
                        model.Installing
                        |> List.map (fun m ->
                            if m.Map.Folder = map && m.State.IsStateSuccess then
                                { m with State = ComMapState.Success prog }
                            else m)
                    { model with Installing = newProg }, Cmd.none
                | MapResult.InstallMapComplete map ->
                    let finished, inProcess = model.Installing |> List.partition (fun m -> m.Map.Folder = map)
                    { model with
                          Installed =
                              finished
                              |> List.append model.Installed
                              |> List.sortBy (fun k -> k.Map.GetName())
                          Installing = inProcess
                          ActiveInstalling = removeFromActive map model.ActiveInstalling }
                    |> addNextActive
                    |> fun (m, cmd) ->
                        m,
                        Cmd.batch
                            [ Cmd.bridgeSend (sender.ConfirmInstall map)
                              cmd ]
                | MapResult.InstallMapError(map, err) ->
                    { model with
                          Installing =
                              (model.Installing
                               |> List.map (fun m ->
                                   if m.Map.Folder = map then { m with State = ComMapState.Error err }
                                   else m))
                          ActiveInstalling = removeFromActive map model.ActiveInstalling }, Cmd.none
            | _ -> model, Cmd.none
        | TabSelected tab -> { model with TabSelected = tab }, Cmd.none
        | ImgSkeleton -> model, Cmd.none
        | Install fName ->
            let startNow = model.ActiveInstalling.Length <= 2
            let newInstalling, newAvailable = partitionComMaps model.Available fName

            let m =
                { model with
                      Available = newAvailable
                      Installed = (model.Installed |> List.filter (fun m -> m.Map.Folder <> fName))
                      Installing = (List.append model.Installing newInstalling)
                      ActiveInstalling =
                          (if startNow then fName :: model.ActiveInstalling
                           else model.ActiveInstalling) }
            match startNow, newInstalling |> List.tryHead with
            | true, Some(h) -> m, Cmd.bridgeSend (sender.Install(createMTarget m h))
            | _ -> m, Cmd.none
        | InstallAll -> model, Cmd.batch (model.Available |> List.map (fun m -> Install(m.Map.Folder) |> Cmd.ofMsg))
        | Update up ->
            match up with
            | InstalledAndNew
            | OnlyInstalled ->
                { model with
                      UpdateSettings = up
                      Updating = true }
            | _ -> { model with UpdateSettings = up }
            |> fun m ->
                if model.Updating then Cmd.none
                else Cmd.ofSub autoUpdate
                |> fun cmd -> m, cmd
        | UpdateMaps ->
            match model.UpdateSettings with
            | InstalledAndNew -> model, Cmd.ofMsg InstallAll
            | OnlyInstalled -> model, Cmd.batch <| getInstalledAvailable model
            | _ -> model, Cmd.none
        | Uninstall s ->
            if model.ActiveUninstalling.IsNone then
                { model with ActiveUninstalling = Some(s) }, Cmd.bridgeSend (sender.Uninstall model.MapsDir.Directory s)
            else { model with Uninstalling = List.append model.Uninstalling [ s ] }, Cmd.none
        | UninstallAll -> model, Cmd.batch (model.Installed |> List.map (fun m -> Uninstall(m.Map.Folder) |> Cmd.ofMsg))
        | CancelInstall s ->
            let cancelled, inProcess = model.Installing |> List.partition (fun m -> m.Map.Folder = s)
            { model with
                  Available =
                      cancelled
                      |> List.map (fun m -> m.Map |> CommunityMapWithState.Init)
                      |> List.append model.Available
                  Installing = inProcess }, Cmd.bridgeSend (sender.Cancel s)
        | CancelInstallAll ->
            model, Cmd.batch (model.Installing |> List.map (fun m -> CancelInstall(m.Map.Folder) |> Cmd.ofMsg))
        | GetInstalled -> model, Cmd.bridgeSend (sender.GetInstalled(model.MapsDir.Directory))
        | GetAvailable ->
            model,
            (if model.Uninstalling.IsEmpty && model.ActiveUninstalling.IsNone then Cmd.bridgeSend (sender.GetAvailable)
             else Cmd.none)
        | ToggleMenu(tab, fName) ->
            match tab with
            | Available -> model, Cmd.none
            | Installed ->
                { model with
                      Installed =
                          (model.Installed
                           |> List.map (fun m ->
                               if m.Map.Folder = fName then { m with MenuState = MenuState.Toggle(m) }
                               else m)) }, Cmd.none
            | Installing ->
                { model with
                      Installing =
                          (model.Installing
                           |> List.map (fun m ->
                               if m.Map.Folder = fName then { m with MenuState = MenuState.Toggle(m) }
                               else m)) }, Cmd.none
