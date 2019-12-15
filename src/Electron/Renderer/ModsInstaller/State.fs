namespace MordhauBuddy.App.ModsInstaller

module State =
    open FSharp.Core // To avoid shadowing Result<_,_>
    open MordhauBuddy.App
    open RenderUtils
    open RenderUtils.Validation
    open Elmish
    open Elmish.Bridge
    open MordhauBuddy.Shared.ElectronBridge
    open BridgeUtils
    open RenderUtils.Directory
    open Types
    open Electron

    let init (uSet: UpdateSettings) =
        { ModsDir =
              { Dir = DirLoad.ModDir
                Directory = ""
                Label = ""
                State = DirState.Init "" }
          UpdateSettings = uSet
          Available = []
          Installed = []
          Installing = []
          UpdatesAvailable = 0
          Uninstalling = []
          Disabling = []
          Enabling = []
          ActiveInstalling = []
          ActiveUninstalling = None
          ActiveDisabling = None
          ActiveEnabling = None
          TabSelected = Available
          Refreshing = false
          Updating = false
          MordhauRunning = false
          Loaded = false }

    [<AutoOpen>]
    module private Helpers =

        let sender = new ModBridgeSender(Caller.ModInstaller)

        let calcAvailableMods (model: Model) =
            model.Available
            |> List.filter (fun mod' ->
                (List.append model.Installed model.Installing)
                |> List.exists (fun m -> m.Mod.Name = mod'.Mod.Name && m.Mod.Version = mod'.Mod.Version)
                |> not)

        let partitionMods (cList: ModWithState list) mod' =
            cList |> List.partition (fun m -> m.Mod.ModId = mod')

        let removeFromActive (modId: int) (activeList: int list) = 
            activeList |> List.filter (fun f -> f <> modId)

        let createMTarget model (cMod: ModWithState) =
            { ModTarget.Directory = model.ModsDir.Directory
              ModTarget.ModInfo = cMod.Mod }

        let addNextActive model =
            if model.ActiveInstalling.Length <= 2 then
                model.Installing
                |> List.filter (fun m ->
                    model.ActiveInstalling
                    |> List.contains m.Mod.ModId
                    |> not)
                |> List.tryHead
            else
                None
            |> function
            | Some m ->
                { model with ActiveInstalling = m.Mod.ModId :: model.ActiveInstalling },
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
                    dispatch UpdateMods
            }
            |> Async.StartImmediate

        let getInstalledAvailable model =
            model.Available
            |> List.choose (fun aMod ->
                if model.Installed |> List.exists (fun iMod -> iMod.Mod.ModId = aMod.Mod.ModId) then
                    aMod.Mod.ModId
                    |> Install
                    |> Cmd.ofMsg
                    |> Some
                else
                    None)

    let update (msg: Msg) (model: Model) =
        match msg with
        | ClientMsg bMsg ->
            match bMsg.BridgeResult with
            | BridgeResult.ModOperation mOp ->
                match mOp with
                | ModOperationResult.DirExists b ->
                    if b then
                        { model with
                              ModsDir = { model.ModsDir with State = DirState.Success "Mods directory located" }
                              Refreshing = true },
                        Cmd.batch
                            [ yield Cmd.ofMsg GetAvailable
                              if model.Refreshing |> not then yield Cmd.ofSub <| autoRefresh GetAvailable GetInstalled ]
                    else
                        { model with ModsDir = { model.ModsDir with State = DirState.Error "Mods directory not found" } },
                        Cmd.none
                | ModOperationResult.Delete(modId, res) ->
                    if model.Uninstalling.IsEmpty then
                        { model with ActiveUninstalling = None }
                    else
                        let next = model.Uninstalling.Head
                        { model with
                              Uninstalling = model.Uninstalling.Tail
                              ActiveUninstalling = Some(next) }
                    |> fun m ->
                        match res with
                        | Ok(_) ->
                            { m with Installed = (m.Installed |> List.filter (fun m -> m.Mod.ModId <> modId)) },
                            Cmd.batch
                                [ yield if m.Uninstalling.IsEmpty
                                           |> not
                                           || m.ActiveUninstalling.IsSome then Cmd.none
                                        else Cmd.ofMsg GetAvailable
                                  match m.ActiveUninstalling with
                                  | Some(s) -> yield Cmd.bridgeSend (sender.Uninstall model.ModsDir.Directory s)
                                  | _ -> () ]
                        | Error(e) ->
                            { m with
                                  Installed =
                                      (m.Installed
                                       |> List.map (fun modState ->
                                           if modState.Mod.ModId = modId then { modState with State = ModState.Error e }
                                           else modState)) }, Cmd.none
                | ModOperationResult.Disable(modId, res) ->
                    if model.Disabling.IsEmpty then
                        { model with ActiveDisabling = None }
                    else
                        let next = model.Disabling.Head
                        { model with
                              Disabling = model.Disabling.Tail
                              ActiveDisabling = Some(next) }
                    |> fun m ->
                        match res with
                        | Ok(_) ->
                            { m with Installed = (m.Installed |> List.map (fun m -> if m.Mod.ModId = modId then { m with Disabled = true } else m)) },
                            Cmd.batch
                                [ yield if m.Disabling.IsEmpty
                                           |> not
                                           || m.ActiveDisabling.IsSome then Cmd.none
                                        else Cmd.ofMsg GetAvailable
                                  match m.ActiveDisabling with
                                  | Some(s) -> yield Cmd.bridgeSend (sender.Disable model.ModsDir.Directory s)
                                  | _ -> () ]
                        | Error(e) ->
                            { m with
                                  Installed =
                                      (m.Installed
                                       |> List.map (fun modState ->
                                           if modState.Mod.ModId = modId then { modState with State = ModState.Error e }
                                           else modState)) }, Cmd.none
                | ModOperationResult.Enable(modId, res) ->
                    if model.Enabling.IsEmpty then
                        { model with ActiveEnabling = None }
                    else
                        let next = model.Enabling.Head
                        { model with
                              Enabling = model.Enabling.Tail
                              ActiveEnabling = Some(next) }
                    |> fun m ->
                        match res with
                        | Ok(_) ->
                            { m with Installed = (m.Installed |> List.map (fun m -> if m.Mod.ModId = modId then { m with Disabled = false } else m)) },
                            Cmd.batch
                                [ yield if m.Enabling.IsEmpty
                                           |> not
                                           || m.ActiveEnabling.IsSome then Cmd.none
                                        else Cmd.ofMsg GetAvailable
                                  match m.ActiveEnabling with
                                  | Some(s) -> yield Cmd.bridgeSend (sender.Enable model.ModsDir.Directory s)
                                  | _ -> () ]
                        | Error(e) ->
                            { m with
                                  Installed =
                                      (m.Installed
                                       |> List.map (fun modState ->
                                           if modState.Mod.ModId = modId then { modState with State = ModState.Error e }
                                           else modState)) }, Cmd.none
                | _ -> model, Cmd.none
            | BridgeResult.Mods mRes ->
                match mRes with
                | ModResult.AvailableMods modList ->
                    let m =
                        { model with
                              Available =
                                  modList
                                  |> List.map ModWithState.Init
                                  |> List.sortBy (fun k -> k.Mod.Name)
                              Loaded = true }
                        |> fun newM ->
                            { newM with
                                  Available = calcAvailableMods newM
                                  UpdatesAvailable = (getInstalledAvailable model).Length }

                    match model.UpdateSettings with
                    | UpdateSettings.Installed -> Cmd.ofMsg UpdateMods
                    | _ -> Cmd.none
                    |> fun cmd -> m, cmd
                | ModResult.InstalledMods (enabledMods, disabledMods) ->
                    let allMods =
                        (enabledMods |> List.map (fun mod' -> ModWithState.Init mod') |> List.sortBy (fun m -> m.Mod.Name))
                        @ (disabledMods |> List.map (fun mod' -> { ModWithState.Init mod' with Disabled = true } ) |> List.sortBy (fun m -> m.Mod.Name))

                    { model with Installed = allMods }
                    |> fun newM -> { newM with Available = calcAvailableMods newM }, Cmd.none
                | ModResult.InstallMod(modId, res) ->
                    match res with
                    | Error(errMsg) ->
                        let setError =
                            model.Installing
                            |> List.map (fun m ->
                                if m.Mod.ModId = modId then { m with State = ModState.Error errMsg }
                                else m)
                        { model with Installing = setError }, Cmd.none
                    | _ -> model, Cmd.none
                | ModResult.InstallModCancelled(modId, b) ->
                    if b then
                        { model with
                              Installing = (model.Installing |> List.filter (fun m -> m.Mod.ModId <> modId))
                              ActiveInstalling = removeFromActive modId model.ActiveInstalling }
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
                                       if m.Mod.ModId = modId then
                                           { m with State = ModState.Error "Error cancelling installation" }
                                       else m)) }, Cmd.none
                | ModResult.InstallModProgress(modId, prog) ->
                    let newProg =
                        model.Installing
                        |> List.map (fun m ->
                            if m.Mod.ModId = modId && m.State.IsStateSuccess then
                                { m with State = ModState.Success prog }
                            else m)
                    { model with Installing = newProg }, Cmd.none
                | ModResult.InstallModComplete modId ->
                    let finished, inProcess = model.Installing |> List.partition (fun m -> m.Mod.ModId = modId)
                    { model with
                          Installed =
                              finished
                              |> List.append model.Installed
                              |> List.sortBy (fun k -> k.Mod.Name)
                          Installing = inProcess
                          ActiveInstalling = removeFromActive modId model.ActiveInstalling }
                    |> addNextActive
                    |> fun (m, cmd) ->
                        m,
                        Cmd.batch
                            [ Cmd.bridgeSend (sender.ConfirmInstall modId)
                              cmd ]
                | ModResult.InstallModError(modId, err) ->
                    { model with
                          Installing =
                              (model.Installing
                               |> List.map (fun m ->
                                   if m.Mod.ModId = modId then { m with State = ModState.Error err }
                                   else m))
                          ActiveInstalling = removeFromActive modId model.ActiveInstalling }, Cmd.none
            | _ -> model, Cmd.none
        | TabSelected tab -> { model with TabSelected = tab }, Cmd.none
        | ImgSkeleton -> model, Cmd.none
        | Install modId ->
            let startNow = model.ActiveInstalling.Length <= 2
            let newInstalling, newAvailable = partitionMods model.Available modId

            let m =
                { model with
                      Available = newAvailable
                      Installed = (model.Installed |> List.filter (fun m -> m.Mod.ModId <> modId))
                      Installing = (List.append model.Installing newInstalling) |> List.sortBy (fun m -> m.Mod.Name, m.Disabled)
                      ActiveInstalling =
                          (if startNow then modId :: model.ActiveInstalling
                           else model.ActiveInstalling) }
            match startNow, newInstalling |> List.tryHead with
            | true, Some(h) -> m, Cmd.bridgeSend (sender.Install(createMTarget m h))
            | _ -> m, Cmd.none
        | InstallAll -> model, Cmd.batch (model.Available |> List.map (fun m -> Install(m.Mod.ModId) |> Cmd.ofMsg))
        | Update up ->
            match up with
            | UpdateSettings.Installed ->
                { model with
                      UpdateSettings = up
                      Updating = true }
            | _ -> { model with UpdateSettings = up }
            |> fun m ->
                if model.Updating then Cmd.none
                else Cmd.ofSub autoUpdate
                |> fun cmd -> m, cmd
        | UpdateMods ->
            match model.UpdateSettings with
            | UpdateSettings.Installed -> model, Cmd.batch <| getInstalledAvailable model
            | _ -> model, Cmd.none
        | Uninstall s ->
            if model.ActiveUninstalling.IsNone then
                { model with ActiveUninstalling = Some(s) }, Cmd.bridgeSend (sender.Uninstall model.ModsDir.Directory s)
            else { model with Uninstalling = List.append model.Uninstalling [ s ] }, Cmd.none
        | Disable s ->
            if model.ActiveDisabling.IsNone then
                { model with ActiveDisabling = Some(s) }, Cmd.bridgeSend (sender.Disable model.ModsDir.Directory s)
            else { model with Disabling = List.append model.Disabling [ s ] }, Cmd.none
        | Enable s ->
            if model.ActiveEnabling.IsNone then
                { model with ActiveEnabling = Some(s) }, Cmd.bridgeSend (sender.Enable model.ModsDir.Directory s)
            else { model with Enabling = List.append model.Enabling [ s ] }, Cmd.none
        | UninstallAll -> model, Cmd.batch (model.Installed |> List.map (fun m -> Uninstall(m.Mod.ModId) |> Cmd.ofMsg))
        | CancelInstall s ->
            let cancelled, inProcess = model.Installing |> List.partition (fun m -> m.Mod.ModId = s)
            { model with
                  Available =
                      cancelled
                      |> List.map (fun m -> m.Mod |> ModWithState.Init)
                      |> List.append model.Available
                  Installing = inProcess }, Cmd.bridgeSend (sender.Cancel s)
        | CancelInstallAll ->
            model, Cmd.batch (model.Installing |> List.map (fun m -> CancelInstall(m.Mod.ModId) |> Cmd.ofMsg))
        | GetInstalled -> model, Cmd.bridgeSend (sender.GetInstalled(model.ModsDir.Directory))
        | GetAvailable ->
            model,
            (if model.Uninstalling.IsEmpty && model.ActiveUninstalling.IsNone then Cmd.bridgeSend (sender.GetAvailable)
             else Cmd.none)
        | ToggleMenu(tab, modId) ->
            match tab with
            | Available -> model, Cmd.none
            | Installed ->
                { model with
                      Installed =
                          (model.Installed
                           |> List.map (fun m ->
                               if m.Mod.ModId = modId then { m with MenuState = MenuState.Toggle(m) }
                               else m)) }, Cmd.none
            | Installing ->
                { model with
                      Installing =
                          (model.Installing
                           |> List.map (fun m ->
                               if m.Mod.ModId = modId then { m with MenuState = MenuState.Toggle(m) }
                               else m)) }, Cmd.none