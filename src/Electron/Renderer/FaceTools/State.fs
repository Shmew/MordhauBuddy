namespace MordhauBuddy.App.FaceTools

module State =
    open FSharp.Core // To avoid shadowing Result<_,_>
    open MordhauBuddy.App
    open RenderUtils
    open RenderUtils.Validation
    open RenderUtils.Directory
    open Elmish
    open Elmish.Bridge
    open MordhauBuddy.Shared.ElectronBridge
    open BridgeUtils
    open RenderUtils.Directory
    open Types

    let init() =
        { Stepper = ChooseProfiles
          StepperComplete = false
          GameDir =
              { Dir = ConfigFile.Game
                Directory = ""
                Label = ""
                State = DirState.Init "" }
          TransferList =
              { LeftProfiles = []
                LeftChecked = 0
                RightProfiles = []
                RightChecked = 0
                State = TransferState.Valid "" }
          TabSelected = Tab.Frankenstein
          ImgLoaded = false
          Import =
              { ImportString = ""
                State = ImportState.Init "You must validate the string before submission" }
          Submit = Submit.Init
          Snack = Snackbar.State.init() }

    let private sender = new INIBridgeSender(Caller.FaceTools)

    let private fileWrap dir =
        { File = ConfigFile.Game
          WorkingDir = Some(dir) }

    let update (msg: Msg) (model: Model) =
        match msg with
        | ClientMsg bRes ->
            match bRes with
            | BridgeResult.INIOperation iOp ->
                match iOp with
                | INIOperationResult.Parse _ -> model, Cmd.bridgeSend sender.GetProfileList
                | INIOperationResult.Backup b ->
                    if b then
                        let profiles = model.TransferList.RightProfiles |> List.map (fun p -> p.Name)
                        match model.TabSelected with
                        | Tab.Frankenstein -> model, Cmd.bridgeSend (sender.SetFrankenstein profiles)
                        | Tab.Random -> model, Cmd.bridgeSend (sender.SetRandom profiles)
                        | Tab.Import -> model, Cmd.bridgeSend (sender.SetCustom profiles model.Import.ImportString)
                        | _ -> { model with Submit = Submit.Error "Invalid submission" }, Cmd.ofMsg SnackDismissMsg
                    else
                        { model with Submit = Submit.Error "Error creating backup" }, Cmd.ofMsg SnackDismissMsg
                | INIOperationResult.CommitChanges b ->
                    (if b then
                        { model with
                              StepperComplete = true
                              Submit = Submit.Success "Changes successfully completed!" }
                     else
                         { model with Submit = Submit.Error "Error commiting changes to the file" }),
                    Cmd.ofMsg SnackDismissMsg
                | _ -> model, Cmd.none
            | BridgeResult.Faces fr ->
                match fr with
                | FaceResult.ProfileList l ->
                    { model with
                          TransferList =
                              if l.Length = 0 then
                                  { model.TransferList with
                                        LeftProfiles = []
                                        LeftChecked = 0
                                        RightProfiles = []
                                        RightChecked = 0
                                        State = TransferState.Error "No profiles found!" }
                              else
                                  { model.TransferList with
                                        LeftProfiles =
                                            l
                                            |> List.map (fun (p, export) ->
                                                { Name = p
                                                  Checked = false
                                                  Export = export })
                                        LeftChecked = 0
                                        RightProfiles = []
                                        RightChecked = 0
                                        State = TransferState.Valid "Please select which profiles you'd like to modify" } },
                    Cmd.none
                | FaceResult.Frankenstein b
                | FaceResult.Random b
                | FaceResult.Custom b ->
                    if b then model, Cmd.bridgeSend (sender.Commit([ fileWrap (model.GameDir.Directory) ]))
                    else { model with Submit = Submit.Error "Modifying INI failed" }, Cmd.ofMsg SnackDismissMsg
            | _ -> model, Cmd.none
        | StepperSubmit ->
            { model with Submit = Submit.Waiting },
            Cmd.bridgeSend (sender.Backup([ fileWrap (model.GameDir.Directory) ]))
        | StepperRestart ->
            { init() with GameDir = model.GameDir }, Cmd.bridgeSend (sender.Parse(fileWrap (model.GameDir.Directory)))
        | StepperNext -> { model with Stepper = model.Stepper.Next }, Cmd.none
        | StepperBack -> { model with Stepper = model.Stepper.Back }, Cmd.none
        | ToggleAll(dir, b) ->
            let toggleAll (iList) = iList |> List.map (fun lItem -> { lItem with Checked = b })

            match dir with
            | Left ->
                let toggles = model.TransferList.LeftProfiles |> toggleAll
                { model with
                      TransferList =
                          { model.TransferList with
                                LeftProfiles = toggles
                                LeftChecked =
                                    if b then toggles.Length
                                    else 0 } }, Cmd.none
            | Right ->
                let toggles = model.TransferList.RightProfiles |> toggleAll
                { model with
                      TransferList =
                          { model.TransferList with
                                RightProfiles = toggles
                                RightChecked =
                                    if b then toggles.Length
                                    else 0 } }, Cmd.none
        | Toggle(dir, pItem) ->
            let toggle (iList: Profile list) =
                iList
                |> List.map (fun p ->
                    if p = pItem then { pItem with Checked = not pItem.Checked }
                    else p)

            let findChecked (model: Model) =
                let checkedCount (pList: Profile List) =
                    pList
                    |> List.filter (fun p -> p.Checked)
                    |> List.length

                let leftChecked = model.TransferList.LeftProfiles |> checkedCount
                let rightChecked = model.TransferList.RightProfiles |> checkedCount

                { model with
                      TransferList =
                          { model.TransferList with
                                LeftChecked = leftChecked
                                RightChecked = rightChecked } }

            match dir with
            | Left ->
                { model with
                      TransferList =
                          { model.TransferList with LeftProfiles = model.TransferList.LeftProfiles |> toggle } }
            | Right ->
                { model with
                      TransferList =
                          { model.TransferList with RightProfiles = model.TransferList.RightProfiles |> toggle } }
            |> findChecked, Cmd.none
        | Move dir ->
            let unCheck (pList: Profile list) = pList |> List.map (fun p -> { p with Checked = false })

            let checkedCount (model: Model) =
                match dir with
                | Left -> { model with TransferList = { model.TransferList with RightChecked = 0 } }
                | Right -> { model with TransferList = { model.TransferList with LeftChecked = 0 } }
            match dir with
            | Left ->
                model.TransferList.RightProfiles
                |> List.partition (fun pItem -> pItem.Checked)
                |> (fun (newLeft, remainRight) ->
                remainRight, model.TransferList.LeftProfiles |> List.append (newLeft |> unCheck))
            | Right ->
                model.TransferList.LeftProfiles
                |> List.partition (fun pItem -> pItem.Checked)
                |> (fun (newRight, remainLeft) ->
                model.TransferList.RightProfiles |> List.append (newRight |> unCheck), remainLeft)
            |> (fun (rList, lList) ->
            { model with
                  TransferList =
                      { model.TransferList with
                            LeftProfiles = lList
                            RightProfiles = rList } }
            |> checkedCount, Cmd.none)
        | TabSelected(tabPicked) -> { model with TabSelected = tabPicked }, Cmd.none
        | ImgSkeleton -> { model with ImgLoaded = true }, Cmd.none
        | SetImportString s ->
            { model with
                  Import =
                      { model.Import with
                            ImportString = s
                            State = ImportState.Init "You must validate the string before submission" } }, Cmd.none
        | ValidateImport ->
            let res = validateImport model.Import.ImportString
            match res with
            | Ok _ -> { model with Import = { model.Import with State = ImportState.Success "Validation successful" } }
            | Error _ -> { model with Import = { model.Import with State = ImportState.Error <| errorStrings res } }
            |> (fun m -> m, Cmd.none)
        | CopiedClipboard -> model, Toastr.success <| Toastr.message "Copied to clipboard!"
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
