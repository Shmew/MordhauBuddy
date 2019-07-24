namespace MordhauBuddy.App

module INITest =
    open System
    open Elmish.React
    open Fable.Core
    open Fable.Core.JsInterop
    open Fable.React
    open Fable.React.Props
    open Fable.MaterialUI
    open Fable.MaterialUI.Core
    open Fable.MaterialUI.Props
    open Fable.MaterialUI.MaterialDesignIcons
    open Fable.MaterialUI.Icons
    open FSharp.Core
    
    //type FileItem =
    //    | Directory of {| Id: int; Name: string; IsOpen: bool; Children: FileItem list |}
    //    | File of {| Id: int; Name: string |}

    //type Model = { Files : FileItem list }

    //type Msg =
    //    | ToggleDirectory of int

    //let init() = {
    //    Files = [
    //        Directory 
    //            {| Id = 1
    //               Name = "Documents"
    //               IsOpen = false 
    //               Children = [
    //                File {| Id = 2; Name = "report.pdf" |}
    //                File {| Id = 3; Name = "image.png" |}
    //                Directory 
    //                    {| Id = 4
    //                       Name = "Programs"
    //                       IsOpen = false
    //                       Children = [ File {| Id = 5; Name = "word.exe" |} ] |}
    //            ]
    //        |}
    //    ]
    //}

    //let rec toggleDirectoryOpened id = function
    //  | File file -> File file
    //  | Directory directory when directory.Id = id ->
    //      Directory {| directory with IsOpen = not directory.IsOpen |}
    //  | Directory directory ->
    //      Directory {| directory with Children = List.map (toggleDirectoryOpened id) directory.Children |}

    //let update (msg: Msg) (model: Model) =
    //  match msg with
    //  | ToggleDirectory id -> { model with Files = List.map (toggleDirectoryOpened id) model.Files }

    //let fileIcon = i [ Class "fa fa-file" ] [ ]
    //let openFolderIcon = i [ Class "fa fa-folder-open" ] [ ]
    //let closedFolderIcon = i [ Class "fa fa-folder" ] [ ]

    //let rec renderFile dispatch = function
    //  | File file ->
    //      AnimatedTree.animatedTree [
    //        AnimatedTree.Key file.Id
    //        AnimatedTree.Icon fileIcon
    //        AnimatedTree.Content (str file.Name)
    //      ]

    //  | Directory directory ->
    //      AnimatedTree.animatedTree [
    //        AnimatedTree.Key directory.Id
    //        AnimatedTree.Icon (if directory.IsOpen then openFolderIcon else closedFolderIcon)
    //        AnimatedTree.Content (str directory.Name)
    //        AnimatedTree.IsOpen directory.IsOpen
    //        AnimatedTree.OnToggled (fun _ -> dispatch (ToggleDirectory directory.Id))
    //        AnimatedTree.Children [ for file in directory.Children -> renderFile dispatch file  ]
    //      ]

    // Domain/Elmish above, view below
    //let private styles (theme : ITheme) : IStyles list = []

    //let private view' (classes: IClasses) model dispatch =
    //    div [] [
    //      //for file in model.Files -> renderFile dispatch file
    //    ]

    //// Workaround for using JSS with Elmish
    //// https://github.com/mvsmal/fable-material-ui/issues/4#issuecomment-422781471
    //type private IProps =
    //    abstract model : Model with get, set
    //    abstract dispatch : (Msg -> unit) with get, set
    //    inherit IClassesProps

    //type private Component(p) =
    //    inherit PureStatelessComponent<IProps>(p)
    //    let viewFun (p : IProps) = view' p.classes p.model p.dispatch
    //    let viewWithStyles = withStyles (StyleType.Func styles) [] viewFun
    //    override this.render() = ReactElementType.create viewWithStyles this.props []

    //let view (model : Model) (dispatch : Msg -> unit) : ReactElement =
    //    let props =
    //        jsOptions<IProps> (fun p ->
    //            p.model <- model
    //            p.dispatch <- dispatch)
    //    ofType<Component, _, _> props []


    let required s =
        if String.IsNullOrEmpty s then Error "Please enter a value"
        else Ok s

    let validInt (str : string) =
        match Int32.TryParse str with
        | true, i -> Ok i
        | false, _ -> Error "Please enter a valid integer"

    let mustBe target i =
        if i = target then Ok i
        else sprintf "Please enter %i" target |> Error

    let validate =
        required
        >> Result.bind validInt
        >> Result.bind (mustBe 42)

    type Model =
        { SimpleText : string
          ValidatedTextRaw : string
          ValidatedTextResult : Result<int, string> }

    // Notes on validation:
    //
    // Raw value should always be in Elmish model to support domain validation,
    // but it's optional to keep the validation result here.
    //
    // Benefits of having validation result in model:
    //   The validation result is only calculated once, so it's more performant
    //   without needing memoization or similar, and there is less chance of
    //   calling the wrong validation function (imagine if you have 10 validated
    //   fields and lots of places in the view function that need the validated value).
    //
    // Drawbacks of having validation result in model:
    //   The validation result represent duplicated state (since it's derivable
    //   from the raw value), so there is increased possibility of invalid state
    //   where ValidatedTextRaw and ValidatedTextResult are not updated together
    //   (not a likely problem here, since it only happens for one message).
    //
    // To avoid the drawbacks above, one could also create separate records for
    // all validated values, containing the raw and validated value, and only
    // creatable through a function that performs the validation. This could lead
    // to lots of types, though.
    type Msg =
        | SetSimpleText of string
        | SetValidatedText of string

    let init() =
        { SimpleText = ""
          ValidatedTextRaw = ""
          ValidatedTextResult = validate "" }

    let update msg m =
        match msg with
        | SetSimpleText s -> { m with SimpleText = s }
        | SetValidatedText s ->
            { m with ValidatedTextRaw = s
                     ValidatedTextResult = validate s }

    // Domain/Elmish above, view below
    let private styles (theme : ITheme) : IStyles list =
        [
            Styles.Custom ("form", [
                FlexWrap "wrap"
            ])
            Styles.Custom ("textField", [
                MarginLeft theme.spacing.unit
                MarginRight theme.spacing.unit
                Width 200
            ])
        ]

    let private view' (classes : IClasses) model dispatch =
        form [ OnSubmit (fun e -> e.preventDefault()); Class classes?form ] [
            textField [
                Class classes?textField
                HTMLAttr.Label "Simple input"
                HTMLAttr.Value model.SimpleText
                DOMAttr.OnChange (fun ev -> ev.Value |> SetSimpleText |> dispatch)
                TextFieldProp.HelperText (sprintf "Current value: %s" model.SimpleText |> str)
            ] []
            textField [
                Class classes?textField
                HTMLAttr.Label "Validated input"
                HTMLAttr.Required true
                HTMLAttr.Value model.ValidatedTextRaw
                MaterialProp.Error model.ValidatedTextResult.IsError
                DOMAttr.OnChange (fun ev -> ev.Value |> SetValidatedText |> dispatch)
                TextFieldProp.HelperText (model.ValidatedTextResult.ErrorOr "Value OK" |> str)
            ] []
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
