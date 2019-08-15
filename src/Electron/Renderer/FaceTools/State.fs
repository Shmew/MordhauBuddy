namespace MordhauBuddy.App.FaceTools

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

    let init(dir: string) =
        { Waiting = true
          ParseWaiting = false
          Stepper = LocateConfig
          StepperComplete = false
          ConfigDir = 
            { Directory = dir
              Error = false
              HelperText = "" 
              Validated = false }
          TransferList =
            { LeftProfiles = []
              LeftChecked = 0
              RightProfiles = []
              RightChecked = 0
              Error = false
              HelperText = "" }
          TabSelected = 0
          Import = 
            { ImportString = ""
              Error = false
              HelperText = "You must validate the string before submission" 
              Validated = false } 
          Submit =
            { Waiting = false
              Error = false
              HelperText = ""
              Complete = false }
          Snack = Snackbar.State.init() }

    let private sender = new INISender(Caller.FaceTools)
    let private fileWrap dir =
        { File = File.Game
          WorkingDir = Some(dir) }

    let update (msg: Msg) (model: Model) =
        let submissionFailed (s: string) =
            { model with
                Submit =
                    { model.Submit with
                        Waiting = false
                        Error = true
                        HelperText = s } }
        match msg with
        | ClientMsg bRes ->
            match bRes with
            | BridgeResult.DefaultDir dOpt ->
                match dOpt with 
                | Some(d) ->
                    { model with
                        Waiting = false
                        ConfigDir =
                            { model.ConfigDir with
                                ConfigDir.Directory = d 
                                ConfigDir.HelperText = "Mordhau directory located"
                                ConfigDir.Validated = false } }, Cmd.ofMsg <| SetConfigDir (d,Ok d)
                | None ->
                    { model with
                        Waiting = false
                        ConfigDir =
                            { model.ConfigDir with
                                ConfigDir.HelperText = "Unable to automatically detect Mordhau directory"
                                ConfigDir.Validated = false } }, Cmd.none
            | BridgeResult.Exists b ->
                { model with
                    Waiting = false
                    ConfigDir =
                        if b then
                            { model.ConfigDir with
                                Error = false
                                HelperText = "Game.ini located"
                                Validated = true } 
                        else
                            { model.ConfigDir with
                                Error = true
                                HelperText = "Game.ini not found"
                                Validated = false } 
                    }, Cmd.none
            | BridgeResult.ProfileList l ->
                { model with
                    ParseWaiting = false
                    Stepper = model.Stepper.Next
                    TransferList =
                        if l.Length = 0 then
                            { model.TransferList with
                                LeftProfiles = []
                                LeftChecked = 0
                                RightProfiles = []
                                RightChecked = 0
                                Error = true
                                HelperText = "No profiles found!"}
                        else
                            { model.TransferList with
                                LeftProfiles =
                                    l |> List.map (fun (p,export) -> { Name = p; Checked = false; Export = export })
                                LeftChecked = 0
                                RightProfiles = []
                                RightChecked = 0
                                Error = false
                                HelperText = "Please select which profiles you'd like to modify"}
                    }, Cmd.none
            | BridgeResult.Parse b ->
                if b then
                    model, Cmd.namedBridgeSend "INI" (sender.getProfileList)
                else
                    { model with
                        Waiting = false
                        ConfigDir =
                            { model.ConfigDir with
                                Error = true
                                HelperText = "Error parsing Game.ini"}}, Cmd.none
            | BridgeResult.Backup b ->
                if b then
                    let profiles =
                        model.TransferList.RightProfiles |> List.map (fun p -> p.Name)
                    match model.TabSelected with
                    | 0 -> model, Cmd.namedBridgeSend "INI" (sender.setFrankenstein profiles)
                    | 1 -> model, Cmd.namedBridgeSend "INI" (sender.setRandom profiles)
                    | 2 -> model, Cmd.namedBridgeSend "INI" (sender.setCustom profiles model.Import.ImportString)
                    | _ -> submissionFailed "Invalid submission", Cmd.ofMsg SnackDismissMsg
                else
                    submissionFailed "Error creating backup", Cmd.ofMsg SnackDismissMsg
            | BridgeResult.Frankenstein b
            | BridgeResult.Random b
            | BridgeResult.Custom b
                ->
                    if b then
                        model, Cmd.namedBridgeSend "INI" (sender.commit(fileWrap(model.ConfigDir.Directory)))
                    else submissionFailed "Modifying INI failed", Cmd.ofMsg SnackDismissMsg
            | BridgeResult.CommitChanges b ->
                if b then
                    { model with
                        StepperComplete = true
                        Submit =
                            { model.Submit with
                                Waiting = false
                                Error = false
                                HelperText = "Changes successfully completed!" } }
                else submissionFailed "Error commiting changes to the file"
                ,Cmd.ofMsg SnackDismissMsg
            | _ -> { model with Waiting = false }, Cmd.none
        | StepperSubmit -> 
            { model with
                Submit =
                    { model.Submit with
                        Waiting = true } },
                Cmd.namedBridgeSend "INI" 
                    (sender.backup(fileWrap(model.ConfigDir.Directory)) )
        | StepperRestart -> 
            { init(model.ConfigDir.Directory) with Waiting = false },
                Cmd.ofMsg <| SetConfigDir (model.ConfigDir.Directory, Ok model.ConfigDir.Directory)
        | StepperNext ->
            match model.Stepper.Next with
            | ChooseProfiles ->
                { model with ParseWaiting = true }, Cmd.namedBridgeSend "INI" 
                    (sender.parse(fileWrap(model.ConfigDir.Directory)))
            | _ ->
                { model with Stepper = model.Stepper.Next }, Cmd.none
        | StepperBack -> { model with Stepper = model.Stepper.Back }, Cmd.none
        | GetDefaultDir ->
            model, Cmd.namedBridgeSend "INI" (sender.defDir)
        | SetConfigDir (s,res) -> 
            match res with
            | Ok s ->
                { model with
                    ConfigDir =
                        { model.ConfigDir with
                            Directory = s
                            Error = false
                            HelperText = "" } },
                Cmd.namedBridgeSend "INI" (sender.exists <| fileWrap(s))
            | Error _ ->
                { model with
                    ConfigDir =
                        { model.ConfigDir with
                            Directory = s
                            Error = true
                            HelperText = errorStrings res } },
                Cmd.none
        | RequestLoad ->
            let handleLoaded =
                function
                | DirSelect.Selected s ->
                    (s, validateConfigDir s)
                    |> SetConfigDir
                | DirSelect.Canceled -> LoadCanceled
            model, Cmd.OfPromise.perform selectDir () handleLoaded
        | LoadCanceled -> model, Cmd.none
        | WaitingStart msg' ->
            { model with Waiting = true }, Cmd.ofMsg msg'
        | ToggleAll(dir,b) ->
            let toggleAll (iList) =
                iList
                |> List.map (fun lItem ->
                    { lItem with Checked = b } )

            match dir with
            | Left ->
                let toggles = model.TransferList.LeftProfiles |> toggleAll
                { model with
                    TransferList = 
                        { model.TransferList with 
                            LeftProfiles = toggles
                            LeftChecked = if b then toggles.Length else 0} }, Cmd.none
            | Right ->
                let toggles = model.TransferList.RightProfiles |> toggleAll
                { model with
                    TransferList = 
                        { model.TransferList with 
                            RightProfiles = toggles
                            RightChecked = if b then toggles.Length else 0} }, Cmd.none
        | Toggle(dir,pItem) ->
            let toggle (iList: Profile list) =
                iList
                |> List.map (fun p ->
                    if p = pItem then
                        { pItem with Checked = not pItem.Checked }
                    else p)
            let findChecked (model: Model) =
                let checkedCount (pList: Profile List) =
                    pList
                    |> List.filter (fun p -> p.Checked)
                    |> List.length
                let leftChecked =
                    model.TransferList.LeftProfiles 
                    |> checkedCount
                let rightChecked =
                    model.TransferList.RightProfiles 
                    |> checkedCount

                { model with
                    TransferList =
                        { model.TransferList with
                            LeftChecked = leftChecked
                            RightChecked = rightChecked }}
            match dir with
            | Left ->
                { model with
                    TransferList =
                        { model.TransferList with
                            LeftProfiles = 
                                model.TransferList.LeftProfiles
                                |> toggle }}
            | Right ->
                { model with
                    TransferList =
                        { model.TransferList with
                            RightProfiles = 
                                model.TransferList.RightProfiles
                                |> toggle }}
            |> findChecked, Cmd.none
        | Move dir ->
            let unCheck (pList: Profile list) =
                pList |> List.map (fun p -> { p with Checked = false })
            let checkedCount (model: Model) =
                match dir with
                | Left ->
                    { model with 
                        TransferList =
                            { model.TransferList with 
                                RightChecked = 0 } }
                | Right ->
                    { model with 
                        TransferList =
                            { model.TransferList with 
                                LeftChecked = 0 } }
            match dir with
            | Left ->
                model.TransferList.RightProfiles
                |> List.partition (fun pItem -> pItem.Checked)
                |> (fun (newLeft,remainRight) ->
                    remainRight,
                    model.TransferList.LeftProfiles 
                    |> List.append (newLeft |> unCheck))
            | Right ->
                model.TransferList.LeftProfiles
                |> List.partition (fun pItem -> pItem.Checked)
                |> (fun (newRight,remainLeft) ->
                    model.TransferList.RightProfiles 
                    |> List.append (newRight |> unCheck), remainLeft)
            |> (fun (rList,lList) ->
                { model with
                    TransferList =
                        { model.TransferList with
                            LeftProfiles = lList
                            RightProfiles = rList } }
                |> checkedCount, Cmd.none)
        | TabSelected(tabPicked) ->
            { model with TabSelected = tabPicked}, Cmd.none
        | SetImportString s ->
            { model with 
                Import = 
                    { model.Import with
                        ImportString = s
                        Validated = false
                        HelperText = 
                            "You must validate the string before submission"} }, Cmd.none
        | ValidateImport ->
            let res = validateImport model.Import.ImportString
            match res with 
            | Ok _ ->
                { model with
                    Import =
                        { model.Import with
                            Error = false
                            Validated = true
                            HelperText = "Validation successful" } }
            | Error _ ->
                { model with
                    Import = 
                        { model.Import with
                            Error = true
                            Validated = false
                            HelperText = errorStrings res } }
            |> (fun m -> m, Cmd.none)
        | CopiedClipboard ->
            model, Toastr.success <| Toastr.message "Copied to clipboard!"
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
