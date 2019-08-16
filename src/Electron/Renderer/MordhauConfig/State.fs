namespace MordhauBuddy.App.MordhauConfig

module State =
    open FSharp.Core  // To avoid shadowing Result<_,_>
    open MordhauBuddy.App
    open RenderUtils
    open RenderUtils.Validation
    open Elmish
    open Elmish.Bridge
    open MordhauBuddy.Shared.ElectronBridge
    open BridgeUtils
    open RenderUtils.Directory
    open Types

    let init(gameDir: string, gameUserDir: string) =
        { Waiting = true
          Complete = false
          Panels = ExpansionPanels.Init()
          EngineDir = 
              { Waiting = false
                Directory = gameDir
                Error = false
                HelperText = "" 
                Validated = false }
          GameUserDir =
              { Waiting = false
                Directory = gameUserDir
                Error = false
                HelperText = "" 
                Validated = false }
          Submit =
              { Waiting = false
                Error = false
                HelperText = ""
                Complete = false }
          Snack = Snackbar.State.init() }

    let sender = new INISender(Caller.MordhauConfig)

    let update (msg: Msg) (model: Model) =
        let submissionFailed (s: string) =
            { model with
                Submit =
                    { model.Submit with
                        Waiting = false
                        Error = true
                        HelperText = s } }
        match msg with
        | ClientMsg bMsg ->
            match bMsg.BridgeResult with
            | BridgeResult.DefaultDir dOpt ->
                match dOpt with 
                | Some(d) ->
                    let mapEngine model =
                        { model with
                            EngineDir =
                                { model.EngineDir with
                                    Directory = d 
                                    HelperText = "Mordhau directory located"
                                    Validated = false } }
                    let mapGameUser model =
                        { model with
                            GameUserDir =
                                { model.GameUserDir with
                                    Directory = d 
                                    HelperText = "Mordhau directory located"
                                    Validated = false } }

                    { model with Waiting = false}
                    |> fun modelWait ->
                        match model.EngineDir.Directory = "", model.GameUserDir.Directory = "" with
                        | true, true -> mapEngine >> mapGameUser
                        | true, false -> mapEngine
                        | false, true -> mapGameUser
                        | _ -> id
                        <| modelWait
                    , Cmd.batch 
                        [ Cmd.ofMsg <| SetConfigDir (d,Ok d, File.GameUserSettings)
                          Cmd.ofMsg <| SetConfigDir (d,Ok d, File.Engine) ]
                | None ->
                    { model with
                        Waiting = false
                        EngineDir =
                            { model.EngineDir with
                                HelperText = "Unable to automatically detect Mordhau directory"
                                Validated = false }
                        GameUserDir =
                            { model.GameUserDir with
                                HelperText = "Unable to automatically detect Mordhau directory"
                                Validated = false } }, Cmd.none
            | BridgeResult.Exists b ->
                match bMsg.File with
                | Some(File.Engine) ->
                    { model with
                        Waiting = false
                        EngineDir =
                            if b then
                                { model.EngineDir with
                                    Waiting = true
                                    Error = false
                                    HelperText = "Engine.ini located"
                                    Validated = true } 
                            else
                                { model.EngineDir with
                                    Error = true
                                    HelperText = "Engine.ini not found"
                                    Validated = false } }, 
                    if b then
                        Cmd.namedBridgeSend "INI"
                            (sender.parse { File = File.Engine; WorkingDir = model.EngineDir.Directory |> Some })
                    else Cmd.none
                | Some(File.GameUserSettings) ->
                    { model with
                        Waiting = false
                        GameUserDir =
                            if b then
                                { model.GameUserDir with
                                    Waiting = true
                                    Error = false
                                    HelperText = "GameUserSettings.ini located"
                                    Validated = true } 
                            else
                                { model.GameUserDir with
                                    Error = true
                                    HelperText = "GameUserSettings.ini not found"
                                    Validated = false } }, 
                    if b then
                        Cmd.namedBridgeSend "INI" 
                            (sender.parse { File = File.GameUserSettings; WorkingDir = model.GameUserDir.Directory |> Some })
                    else Cmd.none
                | _ -> model, Cmd.none
            | BridgeResult.Parse b ->
                match bMsg.File, b with
                | Some(f), false ->
                    match f with 
                    | File.Engine ->
                        { model with
                            Waiting = false
                            EngineDir =
                                { model.EngineDir with
                                    Waiting = false
                                    Error = true
                                    HelperText = "Error parsing Engine.ini" }}
                        , Cmd.none
                    | File.GameUserSettings ->
                        { model with
                            Waiting = false
                            GameUserDir =
                                { model.GameUserDir with
                                    Waiting = false
                                    Error = true
                                    HelperText = "Error parsing GameUserSettings.ini" }}
                        , Cmd.none
                    | _ -> { model with Waiting = false }, Cmd.none
                | Some(_), true when model.GameUserDir.Waiting && model.EngineDir.Waiting ->
                        model,
                        Cmd.namedBridgeSend "INI" 
                            (sender.getConfigs (model.Panels |> List.collect (fun p -> p.Items)))
                | _ -> { model with Waiting = false }, Cmd.none
            | BridgeResult.Backup b ->
                if b then
                    let allPanels =
                        model.Panels |> List.collect (fun p -> p.Items)
                    model, Cmd.namedBridgeSend "INI" (sender.mapConfigs allPanels)
                else
                    submissionFailed "Error creating backup", Cmd.ofMsg SnackDismissMsg
            | BridgeResult.CommitChanges b ->
                if b then
                    { model with
                        Complete = true
                        Submit =
                            { model.Submit with
                                Waiting = false
                                Error = false
                                HelperText = "Changes successfully completed!" } }
                else submissionFailed "Error commiting changes to the file"
                , Cmd.ofMsg SnackDismissMsg
            | BridgeResult.Config cr ->
                match cr with
                | ConfigResult.GetConfigs (oList) ->
                    { model with
                        Waiting = false
                        EngineDir = 
                            { model.EngineDir with
                                Waiting = false }
                        GameUserDir = 
                            { model.GameUserDir with
                                Waiting = false }
                        Panels =
                            model.Panels 
                            |> List.map (fun (p: Panel) ->
                                p.Panel.Modifications
                                |> List.map (fun optGroup ->
                                    oList |> List.tryFind (fun nOptGroup -> optGroup.Title = nOptGroup.Title && optGroup.File = nOptGroup.File)
                                    |> function
                                    | Some(nOptGroup) -> nOptGroup
                                    | _ -> optGroup )
                                |> fun mods -> {p with Items = mods} ) }
                    , Cmd.none
                | ConfigResult.MapConfigs b ->
                    if b then
                        model, Cmd.batch 
                            [ Cmd.namedBridgeSend "INI" (sender.commit { File = File.Engine; WorkingDir = model.EngineDir.Directory |> Some }) 
                              Cmd.namedBridgeSend "INI" (sender.commit { File = File.GameUserSettings; WorkingDir = model.GameUserDir.Directory |> Some }) ]
                    else submissionFailed "Modifying INI failed", Cmd.ofMsg SnackDismissMsg
            | _ -> { model with Waiting = false }, Cmd.none
        | GetDefaultDir ->
            model, Cmd.namedBridgeSend "INI" (sender.defDir)
        | SetConfigDir (s,res,file) -> 
            match res,file with
            | Ok s, File.Engine ->
                { model with
                    EngineDir =
                        { model.EngineDir with
                            Directory = s
                            Error = false
                            HelperText = "" } }
                , Cmd.namedBridgeSend "INI" (sender.exists { File = File.Engine; WorkingDir = Some(s) })
            | Ok s, File.GameUserSettings ->
                { model with
                    GameUserDir =
                        { model.GameUserDir with
                            Directory = s
                            Error = false
                            HelperText = "" } }
                , Cmd.namedBridgeSend "INI" (sender.exists { File = File.GameUserSettings; WorkingDir = Some(s) })
            | Error _, File.Engine ->
                { model with
                    EngineDir =
                        { model.EngineDir with
                            Directory = s
                            Error = true
                            HelperText = errorStrings res } }
                , Cmd.none
            | Error _, File.GameUserSettings ->
                { model with
                    GameUserDir =
                        { model.GameUserDir with
                            Directory = s
                            Error = true
                            HelperText = errorStrings res } }
                , Cmd.none
            | _ -> model, Cmd.none
        | RequestLoad (file) ->
            let handleLoaded =
                function
                | DirSelect.Selected s ->
                    SetConfigDir (s, validateConfigDir s, file)
                | DirSelect.Canceled -> LoadCanceled
            model, Cmd.OfPromise.perform selectDir () handleLoaded
        | LoadCanceled -> model, Cmd.none
        | Expand(p) ->
            model.Panels
            |> List.map (fun oPanel -> 
                if oPanel.Panel.GetTag = p.Panel.GetTag then
                    { p with Expanded = not p.Expanded }
                else { oPanel with Expanded = false })
            |> fun newPanels ->    
                { model with Panels = newPanels}, Cmd.none
        | ToggleOption(oGroup) ->
            let newValues =
                { oGroup with
                    Settings =
                        oGroup.Settings
                        |> List.map (fun s ->
                            if s.Value.IsNone then { s with Value = s.Default |> Some }
                            else { s with Value = None })
                    Enabled = oGroup.Enabled |> not }
            { model with
                Panels =
                    model.Panels 
                    |> List.map (fun p ->
                        { p with 
                            Items = 
                                p.Items 
                                |> List.map(fun s -> 
                                    if s.Title = oGroup.Title then newValues else s) } ) }
            , Cmd.none
        | Submit ->
            { model with
                Submit =
                    { model.Submit with
                        Waiting = true } }
            , Cmd.batch 
                [ Cmd.namedBridgeSend "INI" (sender.backup { File = File.Engine; WorkingDir = model.EngineDir.Directory |> Some }) 
                  Cmd.namedBridgeSend "INI" (sender.backup { File = File.GameUserSettings; WorkingDir = model.GameUserDir.Directory |> Some }) ]
        | SnackMsg msg' ->
            let m, cmd, actionCmd = Snackbar.State.update msg' model.Snack
            { model with Snack = m },
            Cmd.batch [ Cmd.map SnackMsg cmd
                        actionCmd ]
        | SnackDismissMsg ->
            let cmd =
                Snackbar.State.create model.Submit.HelperText
                |> Snackbar.State.withDismissAction "OK"
                |> Snackbar.State.withTimeout 80000
                |> Snackbar.State.add
            model, Cmd.map SnackMsg cmd
