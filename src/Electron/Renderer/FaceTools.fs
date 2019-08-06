namespace MordhauBuddy.App

module rec FaceTools =
    open System
    open Fable.Core
    open Fable.Core.JsInterop
    open Fable.React
    open Fable.React.Props
    open Fable.MaterialUI
    open Fable.MaterialUI.Core
    open Fable.MaterialUI.Props
    open Fable.MaterialUI.MaterialDesignIcons
    open Fable.MaterialUI.Icons
    open FSharp.Core  // To avoid shadowing Result<_,_>
    open RenderUtils
    open RenderUtils.Validation
    open Elmish
    open Elmish.Bridge
    open Elmish.React
    open MordhauBuddy.Shared.ElectronBridge
    open BridgeUtils
    open RenderUtils.Directory
    open Microsoft.FSharp.Reflection
    open Electron

    type ToggleDirection =
        | Left
        | Right

    type Msg =
        | ClientMsg of BridgeResult
        | StepperSubmit
        | StepperRestart
        | StepperNext
        | StepperBack
        | GetDefaultDir
        | SetConfigDir of string * Result<string,string list>
        | RequestLoad
        | LoadCanceled
        | WaitingStart of Msg
        | ToggleAll of ToggleDirection * bool
        | Toggle of ToggleDirection * Profile
        | Move of ToggleDirection
        | TabSelected of int
        | SetImportString of string
        | ValidateImport
        | CopiedClipboard
        | SnackMsg of Snack.Msg<Msg>
        | SnackDismissMsg

    let trySavedConfigDir () =
        match ElectronStore.store.get("configDir", defaultValue = "") |> string with
        | "" -> None
        | s -> Some(s)

    type Steps =
        | LocateConfig
        | ChooseProfiles
        | ChooseAction
        member this.Text =
            this.ToString()
            |> String.duToTitle

        member this.StepCaption =
            match this with
            | LocateConfig -> [ str "Find where your configuration files are located." ]
            | ChooseProfiles -> [ str "Choose which profiles you'd like to modify." ]
            | ChooseAction ->
                [ div [] [str "Choose the type of action you'd like to make."]
                  div [ Style [ CSSProp.PaddingTop "1em" ] ] [
                    str "A backup will be created of your Game.ini file in a \
                        child folder of that directory if modifying settings." 
                  ]
                ]

        static member private Cases =
            FSharpType.GetUnionCases typeof<Steps>

        static member private Instantiate name =
            Steps.Cases
            |> Array.tryFind (fun uc -> uc.Name = name)
            |> Option.map (fun uc -> 
                Reflection.FSharpValue.MakeUnion( uc, [||] ) :?> Steps)
            |> Option.get

        static member GetSteps =
            Steps.Cases
            |> Array.map (fun uc ->
                uc.Name |> Steps.Instantiate)

        member this.StepValue =
            Steps.Cases
            |> Seq.tryFind (fun uc -> uc.Name = this.ToString())
            |> Option.map (fun uc -> uc.Tag)
            |> Option.get

        static member First =
            Steps.Cases
            |> Array.head
            |> (fun t -> t.Name) 
            |> Steps.Instantiate 

        member this.Last =
            Steps.Cases 
            |> Array.last 
            |> (fun t -> t.Name) 
            |> Steps.Instantiate 
            |> (=) this
            
        member this.Next =
            this.StepValue + 1
            |> Array.tryItem <| Steps.Cases
            |> function
            | Some(i) -> i.Name
            | None -> 
                Steps.Cases
                |> Array.head
                |> (fun t -> t.Name)
            |> Steps.Instantiate

        member this.Back =
            this.StepValue - 1
            |> Array.tryItem <| Steps.Cases
            |> function
            | Some(i) -> i.Name
            | None -> 
                Steps.Cases
                |> Array.head
                |> (fun t -> t.Name)
            |> Steps.Instantiate

        member this.Buttons dispatch model =
                match model.StepperComplete with
                | false ->
                    let isDisabled =
                        match this with
                        | LocateConfig when not model.ConfigDir.Validated -> true
                        | ChooseProfiles when model.TransferList.RightProfiles.Length = 0 -> true
                        | ChooseAction when (not model.Import.Validated && model.TabSelected = 2) || model.TabSelected = 3 -> true
                        | _ when model.Waiting -> true
                        | _ -> false

                    [
                        button [
                            HTMLAttr.Disabled (this.StepValue = 0)
                            DOMAttr.OnClick <| fun _ -> dispatch (StepperBack)
                        ] [ str "Back" ]
                        button [
                            HTMLAttr.Disabled isDisabled
                            ButtonProp.Variant ButtonVariant.Contained
                            MaterialProp.Color ComponentColor.Primary
                            DOMAttr.OnClick <| fun _ -> 
                                dispatch (if this.Last then StepperSubmit else StepperNext)
                            Style [ CSSProp.MaxHeight "2.6em" ]
                        ] [ 
                            if model.Submit.Waiting then yield circularProgress [ Style [ CSSProp.MaxHeight "2.6em" ] ]
                            else yield str <| if this.Last then "Submit" else "Next"
                        ]
                    ]
                | true ->
                    [
                        button [
                            DOMAttr.OnClick <| fun _ -> dispatch (StepperRestart)
                        ] [ str "Restart" ]
                    ]
                |> div [ 
                    Style [
                        CSSProp.PaddingTop (string "20px") 
                        CSSProp.MarginTop "auto"
                        CSSProp.MarginLeft "auto"
                    ] 
                ]

        member this.StepElems complete =
            let isComplete i =
                match complete,this.StepValue > i with
                | true, _ -> true
                | _, true -> true
                | _ -> false

            Steps.GetSteps
            |> Array.map (fun stepCase ->
                step [ StepProp.Completed <| isComplete stepCase.StepValue ] [
                    stepLabel [] [str stepCase.Text]
                ])

    type Profile = 
        { Name : string
          Checked : bool
          Export : string }

    type TransferList =
        { LeftProfiles : Profile list
          LeftChecked : int
          RightProfiles : Profile list
          RightChecked : int 
          Error : bool
          HelperText : string }

    type ConfigDir =
        { Directory : string
          Error : bool
          HelperText : string
          Validated : bool }

    type ImportStr =
        { ImportString : string
          Error : bool
          HelperText : string
          Validated : bool }

    type Submit =
        { Waiting : bool
          Error : bool
          HelperText : string
          Complete : bool }

    type Model = 
        { Waiting : bool
          ParseWaiting : bool
          Stepper : Steps
          StepperComplete : bool
          ConfigDir : ConfigDir 
          TransferList : TransferList
          TabSelected : int
          Import : ImportStr
          Submit : Submit
          Snack : Snack.Model<Msg> }

    let init() =
        { Waiting = true
          ParseWaiting = false
          Stepper = LocateConfig
          StepperComplete = false
          ConfigDir = 
            { Directory = (string (defaultArg (trySavedConfigDir()) ""))
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
          Snack = Snack.init() }

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
                    model, Cmd.namedBridgeSend "INI" (INI.Faces.getProfileList)
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
                    | 0 -> model, Cmd.namedBridgeSend "INI" (INI.Faces.setFrankenstein profiles)
                    | 1 -> model, Cmd.namedBridgeSend "INI" (INI.Faces.setRandom profiles)
                    | 2 -> model, Cmd.namedBridgeSend "INI" (INI.Faces.setCustom profiles model.Import.ImportString)
                    | _ -> submissionFailed "Invalid submission", Cmd.ofMsg SnackDismissMsg
                else
                    submissionFailed "Error creating backup", Cmd.ofMsg SnackDismissMsg
            | BridgeResult.Frankenstein b
            | BridgeResult.Random b
            | BridgeResult.Custom b
                ->
                    if b then
                        model, Cmd.namedBridgeSend "INI"
                            (INI.Ops.commit({File = "Game.ini"; WorkingDir = Some(model.ConfigDir.Directory) }))
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
            ElectronStore.store.set("configDir",model.ConfigDir.Directory)
            { model with
                Submit =
                    { model.Submit with
                        Waiting = true } },
                Cmd.namedBridgeSend "INI" 
                    (INI.Ops.backup({File = "Game.ini"; WorkingDir = Some(model.ConfigDir.Directory) }) )
        | StepperRestart -> 
            { init() with Waiting = false },
                Cmd.ofMsg <| SetConfigDir (model.ConfigDir.Directory, Ok model.ConfigDir.Directory)
        | StepperNext ->
            match model.Stepper.Next with
            | ChooseProfiles ->
                { model with ParseWaiting = true }, Cmd.namedBridgeSend "INI" 
                    (INI.Ops.parse({ File = "Game.ini"; WorkingDir = Some(model.ConfigDir.Directory) } ))
            | _ ->
                { model with Stepper = model.Stepper.Next }, Cmd.none
        | StepperBack -> { model with Stepper = model.Stepper.Back }, Cmd.none
        | GetDefaultDir ->
            model, Cmd.namedBridgeSend "INI" (INI.Ops.defDir)
        | SetConfigDir (s,res) -> 
            match res with
            | Ok s ->
                { model with
                    ConfigDir =
                        { model.ConfigDir with
                            Directory = s
                            Error = false
                            HelperText = "" } },
                Cmd.namedBridgeSend "INI" (INI.Ops.exists { File = "Game.ini"; WorkingDir = Some(s) })
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
            let m, cmd, actionCmd = Snack.update msg' model.Snack
            { model with Snack = m },
            Cmd.batch [ Cmd.map SnackMsg cmd
                        actionCmd ]
        | SnackDismissMsg ->
            let cmd =
                Snack.create model.Submit.HelperText
                |> Snack.withDismissAction "Okay"
                |> Snack.withTimeout 80000
                |> Snack.add
            model, Cmd.map SnackMsg cmd

    // Domain/Elmish above, view below
    let private styles (theme : ITheme) : IStyles list = [
        Styles.Custom ("darkList", [
            CSSProp.BackgroundColor theme.palette.background.``default``
        ])
    ]

    let private view' (classes: IClasses) model dispatch =
        let isLocateConfig =
            match model.Stepper with
            | LocateConfig -> true
            | _ -> false
        let content =
            match model.Stepper with
            | LocateConfig ->
                paper [] [
                    div [
                        Style [
                            CSSProp.Padding "2em 5em"
                            CSSProp.Display DisplayOptions.Flex
                            CSSProp.MinHeight "76px"
                        ]
                    ] [
                        textField [
                            HTMLAttr.Label "Mordhau Config Directory"
                            MaterialProp.FullWidth true
                            HTMLAttr.Value model.ConfigDir.Directory
                            MaterialProp.Error model.ConfigDir.Error
                            TextFieldProp.Variant TextFieldVariant.Outlined
                            TextFieldProp.HelperText (model.ConfigDir.HelperText |> str)
                        ] []
                        button [
                            ButtonProp.Variant ButtonVariant.Contained
                            MaterialProp.Color ComponentColor.Secondary
                            DOMAttr.OnClick <| fun _ -> dispatch RequestLoad
                            Style [
                                CSSProp.MarginBottom "20px"
                                CSSProp.MarginLeft "5px" 
                            ]
                        ] [ str "Select" ]
                    ]
                ]
            | ChooseProfiles ->
                let createCard (direction: ToggleDirection) =
                    let checkNum,profiles,headerTitle =
                        match direction with
                        | Left ->
                            model.TransferList.LeftChecked,
                            model.TransferList.LeftProfiles,
                            "Profile List"
                        | Right ->
                            model.TransferList.RightChecked,
                            model.TransferList.RightProfiles,
                            "Modification List"
                    let pLen = profiles.Length
                    grid [ 
                        GridProp.Item true
                        Style [ CSSProp.Padding "0 2em" ]
                    ] [
                        card [
                            Style [ CSSProp.MinWidth "20em" ]
                        ] [
                            cardHeader [
                                checkbox [
                                    DOMAttr.OnClick <| fun _ ->
                                        (direction, checkNum = 0 && checkNum <> pLen) |> ToggleAll |> dispatch
                                    HTMLAttr.Checked (checkNum = pLen && pLen <> 0)
                                    CheckboxProp.Indeterminate (checkNum <> pLen && checkNum > 0)
                                    HTMLAttr.Disabled (pLen = 0)
                                ]
                                |> CardHeaderProp.Avatar
                                CardHeaderProp.Title <| str headerTitle
                                CardHeaderProp.Subheader (sprintf "%i/%i" checkNum pLen |> str)
                            ] []
                            divider []
                            list [
                                MaterialProp.Dense true
                                Style [ 
                                    CSSProp.OverflowY "scroll"
                                    CSSProp.Height "20em"
                                ]
                            ] [
                                for pItem in profiles do
                                    yield
                                        listItem [
                                            ListItemProp.Button true
                                            MaterialProp.DisableRipple true
                                            DOMAttr.OnClick <| fun _ -> dispatch (Toggle(direction,pItem))
                                        ] [
                                            listItemIcon [] [
                                                checkbox [
                                                    HTMLAttr.Checked pItem.Checked
                                                    MaterialProp.DisableRipple true
                                                    Style [ CSSProp.BackgroundColor "transparent"]
                                                ]
                                            ]
                                            listItemText [] [
                                                str pItem.Name
                                            ]
                                        ]
                            ]
                        ]
                    ]

                grid [
                    GridProp.Container true
                    GridProp.Spacing GridSpacing.``0``
                    GridProp.Justify GridJustify.Center
                    GridProp.AlignItems GridAlignItems.Center
                    Style [ CSSProp.Width "unset" ]
                ] [
                    createCard Left
                    grid [ 
                        GridProp.Container true
                        GridProp.Direction GridDirection.Column
                        GridProp.AlignItems GridAlignItems.Center
                        Style [ CSSProp.Width "unset" ]
                    ] [
                        button [
                            ButtonProp.Variant ButtonVariant.Outlined
                            ButtonProp.Size ButtonSize.Small
                            DOMAttr.OnClick <| fun _ -> dispatch (Move(Right))
                            HTMLAttr.Disabled (model.TransferList.LeftChecked = 0)
                        ] [ str ">" ]
                        button [
                            ButtonProp.Variant ButtonVariant.Outlined
                            ButtonProp.Size ButtonSize.Small
                            DOMAttr.OnClick <| fun _ -> dispatch (Move(Left))
                            HTMLAttr.Disabled (model.TransferList.RightChecked = 0)
                        ] [ str "<" ]
                    ]
                    createCard Right
                ]
            | ChooseAction ->
                let tabDescription =
                    match model.TabSelected with
                    | 0 ->
                        str "Frankenstein mode is done automatically \
                            and will select at random the maximum or \
                            minimum allowed values for each aspect of \
                            the character's face."
                    | 1 -> 
                        str "Random mode is done automatically and \
                            will select at random all aspects of the \
                            character's face." 
                    | 2 -> 
                        str "Import mode enables you to set faces from a face value string."
                    | _ ->
                        str "Export lets you generate string values of the profiles you've selected."

                let tabContent =
                    match model.TabSelected with
                    | 0 ->
                        grid [
                            GridProp.Container true
                            GridProp.Spacing GridSpacing.``0``
                            GridProp.Direction GridDirection.Column
                            GridProp.AlignItems GridAlignItems.Center
                            Style [ 
                                CSSProp.Padding "1em 5em"
                                CSSProp.Display DisplayOptions.Flex
                            ] 
                        ] [
                            img [
                                HTMLAttr.Src (stat "frankenstein.png")
                                Style [ CSSProp.BorderRadius "4px" ]
                            ]
                        ]
                    | 2 ->
                        div [] [
                            div [ 
                                Style [ 
                                    CSSProp.Padding "2em 5em"
                                    CSSProp.Display DisplayOptions.Flex
                                ] 
                            ] [
                                textField [
                                    TextFieldProp.Variant TextFieldVariant.Outlined
                                    MaterialProp.FullWidth true
                                    HTMLAttr.Label "Import string"
                                    HTMLAttr.Placeholder "Paste import string here"
                                    HTMLAttr.Value model.Import.ImportString
                                    MaterialProp.Color ComponentColor.Secondary
                                    DOMAttr.OnChange (fun ev -> dispatch <| SetImportString(ev.Value) )
                                    MaterialProp.Error model.Import.Error
                                    TextFieldProp.HelperText (model.Import.HelperText |> str)
                                ] []
                                button [
                                    HTMLAttr.Disabled model.Import.Validated
                                    ButtonProp.Variant ButtonVariant.Contained
                                    MaterialProp.Color ComponentColor.Secondary
                                    DOMAttr.OnClick <| fun _ -> dispatch ValidateImport
                                    Style [ CSSProp.MarginLeft "1em"; CSSProp.MaxHeight "4em" ]
                                ] [ str "Validate" ]
                            ]
                            div [ Style [ CSSProp.Padding "0em 5em" ] ] [
                                expansionPanel [
                                    MaterialProp.Elevation 2
                                ] [
                                    expansionPanelSummary [
                                        ExpansionPanelSummaryProp.ExpandIcon <| expandMoreIcon []
                                    ] [ str "An example of an import string" ]
                                    divider []
                                    expansionPanelDetails [
                                        Style [ CSSProp.PaddingRight "0em" ]
                                    ] [
                                        code [
                                            Style [ 
                                                CSSProp.WordBreak "break-all"
                                            ]
                                        ] [
                                            str Samples.faceImport
                                        ]
                                        button [
                                            DOMAttr.OnClick <| fun _ ->
                                                renderer.clipboard.writeText(Samples.faceImport)
                                                dispatch CopiedClipboard
                                            Style [ 
                                                CSSProp.MaxHeight "4em"
                                                CSSProp.MaxWidth "3em"
                                                CSSProp.Float "right"
                                                CSSProp.MarginRight "0em"
                                            ]
                                        ] [ clipboardTextIcon [] ]

                                    ]
                                ]
                            ]
                        ]
                    | 3 ->
                        let profileInd =
                            model.TransferList.RightProfiles
                            |> List.indexed
                            |> Seq.ofList
                        div [ ] [
                            div [ 
                                Style [ 
                                    CSSProp.Padding "2em 5em"
                                    CSSProp.Display DisplayOptions.Flex
                                ] 
                            ] [ 
                                list [
                                    Class classes?darkList
                                    Style [ 
                                        CSSProp.FlexGrow 1 
                                        CSSProp.MaxHeight "20em"
                                        CSSProp.OverflowY "scroll"
                                    ]
                                ] [
                                    for (index,profile) in profileInd do
                                        yield
                                            div [] [
                                                listItem [] [
                                                    listItemText [] [
                                                        str profile.Name
                                                    ]
                                                    listItemSecondaryAction [] [
                                                        button [
                                                            DOMAttr.OnClick <| fun _ ->
                                                                renderer.clipboard.writeText(profile.Export)
                                                                dispatch CopiedClipboard
                                                            Style [ 
                                                                CSSProp.MaxHeight "4em"
                                                                CSSProp.MaxWidth "3em"
                                                                CSSProp.Float "right"
                                                                CSSProp.MarginRight "0em"
                                                            ]
                                                        ] [ clipboardTextIcon [] ]
                                                    ]
                                                ]
                                                divider [
                                                    HTMLAttr.Hidden (index + 1 = model.TransferList.RightProfiles.Length)
                                                ]
                                            ]
                                ]
                            ]
                        ]
                    | _ -> div [] []

                paper [] [
                    tabs [
                        HTMLAttr.Value (model.TabSelected)
                        TabsProp.Variant TabsVariant.FullWidth
                        TabsProp.ScrollButtons ScrollButtonsType.On
                        TabsProp.IndicatorColor TabsIndicatorColor.Secondary
                        TabsProp.TextColor TabsTextColor.Secondary
                        TabsProp.Centered true
                        TabsProp.OnChange (fun _ tabPicked -> dispatch <| TabSelected(tabPicked) )
                    ] [
                        tab [ HTMLAttr.Label "Frankenstein" ]
                        tab [ HTMLAttr.Label "Random" ]
                        tab [ HTMLAttr.Label "Import" ]
                        tab [ HTMLAttr.Label "Export" ]
                    ]
                    divider []
                    div [
                        Style [ CSSProp.Padding "2em"; CSSProp.MinHeight "10em" ]
                    ] [ 
                        typography [] [ tabDescription ]
                        tabContent
                    ]
                ]
        div [
            Style [
                CSSProp.FlexDirection "column"
                CSSProp.Display DisplayOptions.Flex
                CSSProp.Height "inherit"
            ]
        ] [
            yield lazyView2 Snack.view model.Snack (SnackMsg >> dispatch)
            yield
                div [
                    Style [
                        CSSProp.FlexDirection "column"
                        CSSProp.Display DisplayOptions.Flex
                        CSSProp.Height "inherit"
                    ]
                ] [
                    stepper [
                        MaterialProp.Elevation 1
                        StepperProp.ActiveStep (model.Stepper.StepValue)
                    ]
                        <| model.Stepper.StepElems model.StepperComplete
            
                    div [ Style [ CSSProp.Padding (string "3em") ] ] [ 
                        match model.Waiting, model.ParseWaiting with
                        | true, false when model.ConfigDir.Directory = "" && isLocateConfig ->
                            yield circularProgress [
                                Style [CSSProp.MarginLeft "45%"]
                                DOMAttr.OnAnimationStart <| fun _ ->
                                    async {
                                        do! Async.Sleep 1000
                                        return dispatch GetDefaultDir
                                    } |> Async.StartImmediate
                            ]
                        | true, false when isLocateConfig ->
                            yield circularProgress [
                                Style [CSSProp.MarginLeft "45%"]
                                DOMAttr.OnAnimationStart <| fun _ ->
                                    async {
                                        do! Async.Sleep 1000
                                        return dispatch <| SetConfigDir
                                            (model.ConfigDir.Directory, validateConfigDir model.ConfigDir.Directory)
                                    } |> Async.StartImmediate
                            ]
                        | _ when model.Waiting || model.ParseWaiting ->
                            yield circularProgress [
                                Style [CSSProp.MarginLeft "45%"]
                            ]
                        | _ -> yield content
                    ]
                    div [ Style [CSSProp.PaddingLeft "3em"] ] model.Stepper.StepCaption
                    model.Stepper.Buttons dispatch model
                ]
            ]

    // Workaround for using JSS with Elmish
    // https://github.com/mvsmal/fable-material-ui/issues/4#issuecomment-422781471
    type private IProps =
        abstract model : Model with get, set
        abstract dispatch : (Msg -> unit) with get, set
        inherit IClassesProps

    type private Component(p) =
        inherit PureStatelessComponent<IProps>(p)
        let viewFun (p : IProps) = view' p.classes p.model p.dispatch
        let viewWithStyles = withStyles (StyleType.Func styles) [] viewFun
        override this.render() = ReactElementType.create viewWithStyles this.props []
            
    let view (model : Model) (dispatch : Msg -> unit) : ReactElement =
        let props =
            jsOptions<IProps> (fun p ->
                p.model <- model
                p.dispatch <- dispatch)
        ofType<Component, _, _> props []
