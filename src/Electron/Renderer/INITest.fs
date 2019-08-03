namespace MordhauBuddy.App

module rec INITest =
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

    let trySavedConfigDir () =
        match ElectronStore.store.get("configDir", defaultValue = "") |> string with
        | "" -> None
        | s -> Some(s)

    type Steps =
        | LocateConfig
        | ChooseProfiles
        | ChooseModification
        member this.Text =
            this.ToString()
            |> String.duToTitle

        member this.StepCaption =
            match this with
            | LocateConfig -> [ str "Find where your configuration files are located." ]
            | ChooseProfiles -> [ str "Choose which profiles you'd like to modify." ]
            | ChooseModification ->
                [ div [] [str "Choose the type of operation you'd like to make."]
                  div [ Style [ CSSProp.PaddingTop "1em" ] ] [
                    str "A backup will be created of your Game.ini file in a \
                        child folder of that directory." 
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
                        [
                            button [
                                HTMLAttr.Disabled (this.StepValue = 0)
                                DOMAttr.OnClick <| fun _ -> dispatch (StepperBack)
                            ] [ str "Back" ]
                            button [
                                HTMLAttr.Disabled 
                                    (not model.ConfigDir.Validated 
                                        || model.TransferList.RightProfiles.Length = 0)
                                ButtonProp.Variant ButtonVariant.Contained
                                MaterialProp.Color ComponentColor.Primary
                                DOMAttr.OnClick <| fun _ -> 
                                    dispatch (if this.Last then StepperSubmit else StepperNext)
                            ] [ str <| if this.Last then "Submit" else "Next" ]
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
                step [StepProp.Completed <| isComplete stepCase.StepValue] [
                    stepLabel [] [str stepCase.Text]
                ])

    type Profile = 
        { Name : string
          Checked : bool }

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

    type Model = 
        { Waiting : bool
          DefaultDirTried : bool
          Stepper : Steps
          StepperComplete : bool
          ConfigDir : ConfigDir 
          TransferList : TransferList
          TabSelected : int }

    let init() =
        { Waiting = true
          DefaultDirTried = false
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
          TabSelected = 0 }

    let update (msg: Msg) (model: Model) =
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
                    Waiting = false
                    Stepper = model.Stepper.Next
                    TransferList =
                        if l.Length = 0 then
                            { model.TransferList with
                                Error = true
                                HelperText = "No profiles found!"}
                        else
                            { model.TransferList with
                                LeftProfiles =
                                    l |> List.map (fun p -> { Name = p; Checked = false })
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
            | _ -> { model with Waiting = false },Cmd.none
        | StepperSubmit -> 
            ElectronStore.store.set("configDir",model.ConfigDir.Directory)
            { model with StepperComplete = true }, Cmd.none
        | StepperRestart -> 
            { init() with Waiting = false; DefaultDirTried = true },
                Cmd.ofMsg <| SetConfigDir (model.ConfigDir.Directory, Ok model.ConfigDir.Directory)
        | StepperNext ->
            match model.Stepper.Next with
            | ChooseProfiles ->
                { model with Waiting = true}, Cmd.namedBridgeSend "INI" 
                    (INI.Ops.parse({ File = "Game.ini"; WorkingDir = Some(model.ConfigDir.Directory) } ))
            | _ ->
                { model with Stepper = model.Stepper.Next }, Cmd.none
        | StepperBack -> { model with Stepper = model.Stepper.Back }, Cmd.none
        | GetDefaultDir ->
            { model with DefaultDirTried = true }, Cmd.namedBridgeSend "INI" (INI.Ops.defDir)
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

    // Domain/Elmish above, view below
    let private styles (theme : ITheme) : IStyles list = [
        Styles.Custom' ("toolbar", theme.mixins.toolbar)
    ]

    let private view' (classes: IClasses) model dispatch =
        let content =
            match model.Stepper, model.Waiting with
            | _, true ->
                str ""
            | LocateConfig, _ ->
                form [ 
                    OnSubmit (fun e -> e.preventDefault())
                    Class classes?form 
                    Style [
                        CSSProp.Display DisplayOptions.Flex
                        CSSProp.MinHeight "76px"
                    ]
                ] [
                    textField [
                        Class classes?textField
                        HTMLAttr.Label "Mordhau Config Directory"
                        MaterialProp.FullWidth true
                        HTMLAttr.Value model.ConfigDir.Directory
                        MaterialProp.Error model.ConfigDir.Error
                        TextFieldProp.Variant TextFieldVariant.Outlined
                        TextFieldProp.HelperText (model.ConfigDir.HelperText |> str)
                    ] []
                    button [
                        ButtonProp.Variant ButtonVariant.Contained
                        MaterialProp.Color ComponentColor.Primary
                        DOMAttr.OnClick <| fun _ -> dispatch RequestLoad
                        Style [
                            CSSProp.MarginBottom "20px"
                            CSSProp.MarginLeft "5px" 
                        ]
                    ] [ str "Select" ]
                ]
            | ChooseProfiles, _ ->
                grid [
                    GridProp.Container true
                    GridProp.Spacing GridSpacing.``40``
                    GridProp.Justify GridJustify.Center
                    GridProp.AlignItems GridAlignItems.Center
                    Style [ CSSProp.Width "unset" ]
                ] [
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
                                        dispatch 
                                            (ToggleAll(Left,
                                                (model.TransferList.LeftChecked = 0 
                                                    && model.TransferList.LeftChecked 
                                                        <> model.TransferList.LeftProfiles.Length) ))
                                    HTMLAttr.Checked 
                                        (model.TransferList.LeftChecked = model.TransferList.LeftProfiles.Length
                                            && model.TransferList.LeftProfiles.Length <> 0)
                                    CheckboxProp.Indeterminate 
                                        ( model.TransferList.LeftChecked <> 
                                          model.TransferList.LeftProfiles.Length && 
                                          model.TransferList.LeftChecked > 0 )
                                    HTMLAttr.Disabled (model.TransferList.LeftProfiles.Length = 0)
                                ]
                                |> CardHeaderProp.Avatar
                                CardHeaderProp.Title <| str "Profile List"
                                CardHeaderProp.Subheader 
                                    (sprintf "%i/%i" model.TransferList.LeftChecked model.TransferList.LeftProfiles.Length |> str)
                            ] []
                            divider []
                            list [
                                MaterialProp.Dense true
                                Style [ 
                                    CSSProp.OverflowY "scroll"
                                    CSSProp.Height "20em"
                                ]
                            ] [
                                for pItem in model.TransferList.LeftProfiles do
                                    yield
                                        listItem [
                                            DOMAttr.OnClick <| fun _ -> dispatch (Toggle(Left,pItem))
                                        ] [
                                            listItemIcon [] [
                                                checkbox [
                                                    HTMLAttr.Checked pItem.Checked
                                                    MaterialProp.DisableRipple true
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
                                        dispatch 
                                            (ToggleAll(Right,
                                                (model.TransferList.RightChecked = 0 
                                                    && model.TransferList.RightChecked 
                                                        <> model.TransferList.RightProfiles.Length) ))
                                    HTMLAttr.Checked 
                                        (model.TransferList.RightChecked = model.TransferList.RightProfiles.Length
                                            && model.TransferList.RightProfiles.Length <> 0)
                                    CheckboxProp.Indeterminate 
                                        ( model.TransferList.RightChecked <>
                                          model.TransferList.RightProfiles.Length &&
                                          model.TransferList.RightChecked > 0 )
                                    HTMLAttr.Disabled (model.TransferList.RightProfiles.Length = 0)
                                ]
                                |> CardHeaderProp.Avatar
                                CardHeaderProp.Title <| str "Modification List"
                                CardHeaderProp.Subheader (
                                    sprintf "%i/%i" model.TransferList.RightChecked model.TransferList.RightProfiles.Length |> str)
                            ] []
                            divider []
                            list [
                                MaterialProp.Dense true
                                Style [ 
                                    CSSProp.OverflowY "scroll"
                                    CSSProp.Height "20em"
                                ]
                            ] [
                                for pItem in model.TransferList.RightProfiles do
                                    yield
                                        listItem [
                                            DOMAttr.OnClick <| fun _ -> dispatch (Toggle(Right,pItem))
                                        ] [
                                            listItemIcon [] [
                                                checkbox [
                                                    HTMLAttr.Checked pItem.Checked
                                                    MaterialProp.DisableRipple true
                                                ]
                                            ]
                                            listItemText [] [
                                                str pItem.Name
                                            ]
                                        ]
                            ]
                        ]
                    ]

                ]
            | ChooseModification, _ ->
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
                    | _ -> 
                        str "Custom mode enables you to manually \
                            select each value of the character's face."

                let tabContent =
                    match model.TabSelected with
                    | 2 -> []
                    | _ -> []

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
                        tab [ HTMLAttr.Label "Custom" ]
                    ]
                    divider []
                    div [
                        Style [ CSSProp.Padding "2em"; CSSProp.MinHeight "10em" ]
                    ] [ 
                        typography [] [ tabDescription ]
                        div [] tabContent
                    ]
                ]

        div [
            Style [
                FlexDirection "column"
                CSSProp.Display DisplayOptions.Flex
                CSSProp.Height "inherit"
            ]
        ] [
            stepper [ActiveStep (model.Stepper.StepValue)]
                <| model.Stepper.StepElems model.StepperComplete
            
            div [Style [ CSSProp.Padding (string "3em") ]] [ 
                match model.Waiting, model.DefaultDirTried with
                | true, false ->
                    yield circularProgress [
                        Style [CSSProp.MarginLeft "45%"]
                        DOMAttr.OnAnimationStart <| fun _ ->
                            async {
                                do! Async.Sleep 1000
                                return dispatch GetDefaultDir
                            } |> Async.StartImmediate
                    ]
                | true, true ->
                    yield circularProgress [
                        Style [CSSProp.MarginLeft "45%"]
                    ]
                | _, _ -> yield content
            ]
            div [ Style [CSSProp.PaddingLeft "3em"] ] model.Stepper.StepCaption
            model.Stepper.Buttons dispatch model
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
