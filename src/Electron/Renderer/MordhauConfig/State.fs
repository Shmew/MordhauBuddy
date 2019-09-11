namespace MordhauBuddy.App.MordhauConfig

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

    let init() =
        { Complete = false
          Panels = ExpansionPanels.Init()
          EngineDir =
              { Dir = DirLoad.ConfigFiles(ConfigFile.Engine)
                Directory = ""
                Label = ""
                State = DirState.Init "" }
          GameUserDir =
              { Dir = DirLoad.ConfigFiles(ConfigFile.GameUserSettings)
                Directory = ""
                Label = ""
                State = DirState.Init "" }
          Submit = Submit.Init
          Snack = Snackbar.State.init() }

    let private sender = new INIBridgeSender(Caller.MordhauConfig)

    let update (msg: Msg) (model: Model) =
        match msg with
        | ClientMsg bMsg ->
            match bMsg.BridgeResult with
            | BridgeResult.INIOperation iOp ->
                match iOp with
                | INIOperationResult.Parse _ ->
                    model, Cmd.bridgeSend (sender.GetConfigs(model.Panels |> List.collect (fun p -> p.Items)))
                | INIOperationResult.Backup b ->
                    if b then
                        let allPanels = model.Panels |> List.collect (fun p -> p.Items)
                        model, Cmd.bridgeSend (sender.MapConfigs allPanels)
                    else
                        { model with Submit = Submit.Error "Error creating backup" }, Cmd.ofMsg SnackDismissMsg
                | INIOperationResult.CommitChanges b ->
                    (if b then { model with Submit = Submit.Success "Changes successfully completed!" }
                     else { model with Submit = Submit.Error "Error commiting changes to the file" }),
                    Cmd.ofMsg SnackDismissMsg
                | _ -> model, Cmd.none
            | BridgeResult.Config cr ->
                match cr with
                | ConfigResult.GetConfigs(oList) ->
                    { model with
                          EngineDir = { model.EngineDir with State = DirState.Init "" }
                          GameUserDir = { model.GameUserDir with State = DirState.Init "" }
                          Panels =
                              model.Panels
                              |> List.map (fun (p: Panel) ->
                                  p.Items
                                  |> List.map (fun optGroup ->
                                      oList
                                      |> List.tryFind (fun nOptGroup -> optGroup.Title = nOptGroup.Title)
                                      |> function
                                      | Some(nOptGroup) -> nOptGroup
                                      | _ -> optGroup
                                      |> (fun mods ->
                                      match mods.Settings |> List.forall (fun setting -> setting.Value.IsSome) with
                                      | true -> { mods with Enabled = true }
                                      | false -> mods))
                                  |> fun mods -> { p with Items = mods }) }, Cmd.none
                | ConfigResult.MapConfigs b ->
                    if b then
                        model,
                        Cmd.bridgeSend
                            (sender.Commit
                                [ { File = ConfigFile.Engine
                                    WorkingDir = model.EngineDir.Directory |> Some }
                                  { File = ConfigFile.GameUserSettings
                                    WorkingDir = model.GameUserDir.Directory |> Some } ])
                    else
                        { model with Submit = Submit.Error "Modifying INI failed" }, Cmd.ofMsg SnackDismissMsg
            | _ -> model, Cmd.none
        | Expand(p) ->
            model.Panels
            |> List.map
                ((fun oPanel ->
                 if oPanel.Panel.GetTag = p.Panel.GetTag then { p with Expanded = not p.Expanded }
                 else { oPanel with Expanded = false })
                 >> (fun oPanel ->
                 { oPanel with
                       Items =
                           oPanel.Items
                           |> List.map (fun i ->
                               if i.Expanded then { i with Expanded = false }
                               else i) }))
            |> fun newPanels -> { model with Panels = newPanels }, Cmd.none
        | ExpandSubPanel(oGroup) ->
            let mapItems (oList: OptionGroup list) =
                oList
                |> List.map (fun i ->
                    if oGroup.Title = i.Title || i.Expanded then { i with Expanded = i.Expanded |> not }
                    else i)

            let mapPanels (pList: Panel list) = pList |> List.map (fun p -> { p with Items = mapItems p.Items })

            { model with Panels = mapPanels model.Panels }, Cmd.none
        | ToggleOption(oGroup) ->
            let toggle = oGroup.Enabled |> not

            let newValues =
                { oGroup with
                      Settings =
                          if toggle then
                              oGroup.Settings
                              |> List.map (fun s ->
                                  if s.Value.IsNone then { s with Value = s.Default |> Some }
                                  else s)
                          else
                              oGroup.Settings
                              |> List.map (fun s ->
                                  if s.Value.IsNone then s
                                  else { s with Value = None })
                      Enabled = toggle }
            { model with
                  Submit =
                      if model.Submit.IsSubmitError then model.Submit
                      else Submit.Init
                  Panels =
                      model.Panels
                      |> List.map (fun p ->
                          { p with
                                Items =
                                    p.Items
                                    |> List.map (fun s ->
                                        if s.Title = oGroup.Title then newValues
                                        else s) }) }, Cmd.none
        | MoveSlider(key, value) ->
            let tryGetValue (f: float) (kValue: KeyValues.Values) =
                match kValue with
                | KeyValues.Values.Float(_) ->
                    f
                    |> KeyValues.Values.Float
                    |> Some
                | KeyValues.Values.Int(_) ->
                    f
                    |> int
                    |> KeyValues.Values.Int
                    |> Some
                | _ -> None
            let mapSettings (sList: KeyValues list) =
                sList
                |> List.map (fun s ->
                    let res = tryGetValue value s.Default
                    if res.IsSome && s.Mutable.IsSome && s.Key = key then { s with Value = res }
                    else s)

            let mapOGroups (oList: OptionGroup list) =
                oList |> List.map (fun o -> { o with Settings = mapSettings o.Settings })

            let newPanels =
                model.Panels
                |> List.map (fun p ->
                    if p.Expanded && (p.Items |> List.exists (fun o -> o.Expanded)) then
                        { p with Items = mapOGroups p.Items }
                    else p)
            { model with
                  Submit = Submit.Init
                  Panels = newPanels }, Cmd.none
        | Submit ->
            { model with Submit = Submit.Waiting },
            Cmd.bridgeSend
                (sender.Backup
                    [ { File = ConfigFile.Engine
                        WorkingDir = model.EngineDir.Directory |> Some }
                      { File = ConfigFile.GameUserSettings
                        WorkingDir = model.GameUserDir.Directory |> Some } ])
        | SnackMsg msg' ->
            let m, cmd, actionCmd = Snackbar.State.update msg' model.Snack
            { model with Snack = m },
            Cmd.batch
                [ Cmd.map SnackMsg cmd
                  actionCmd ]
        | SnackDismissMsg ->
            match model.Submit with
            | Submit.Success helperText
            | Submit.Error helperText ->
                let cmd =
                    Snackbar.State.create helperText
                    |> Snackbar.State.withDismissAction "OK"
                    |> Snackbar.State.withTimeout 80000
                    |> Snackbar.State.add
                model, Cmd.map SnackMsg cmd
            | _ -> model, Cmd.none
