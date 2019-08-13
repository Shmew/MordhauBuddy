namespace MordhauBuddy.App.EngineTools

module View =
    open Fable.Core.JsInterop
    open Fable.React
    open Fable.React.Props
    open Fable.MaterialUI
    open Fable.MaterialUI.Core
    open Fable.MaterialUI.MaterialDesignIcons
    open Fable.MaterialUI.Icons
    open FSharp.Core  /// To avoid shadowing Result<_,_>
    open MordhauBuddy.App
    open RenderUtils
    open RenderUtils.Validation
    open Elmish.React
    open Electron
    open Types

    let private styles (theme : ITheme) : IStyles list = [
        Styles.Custom ("darkList", [
            CSSProp.BackgroundColor theme.palette.background.``default``
        ])
    ]

    let private config  (classes: IClasses) model dispatch =
        paper [] [
            div [
                Style [
                    CSSProp.Padding "2em 5em"
                    CSSProp.Display DisplayOptions.Flex
                    CSSProp.MinHeight "76px"
                ]
            ] [
                textField [
                    TextFieldProp.Variant TextFieldVariant.Outlined
                    MaterialProp.FullWidth true
                    HTMLAttr.Label "Mordhau Game.ini Directory"
                    HTMLAttr.Value model.ConfigDir.Directory
                    MaterialProp.Error model.ConfigDir.Error
                    TextFieldProp.HelperText (model.ConfigDir.HelperText |> str)
                ] []
                button [
                    ButtonProp.Variant ButtonVariant.Contained
                    MaterialProp.Color ComponentColor.Secondary
                    DOMAttr.OnClick <| fun _ -> dispatch RequestLoad
                    Style [
                        CSSProp.MarginLeft "1em" 
                        CSSProp.MaxHeight "4em" 
                    ]
                ] [ str "Select" ]
            ]
        ]

    let private content (classes: IClasses) model dispatch =
        config classes model dispatch

    let private view' (classes: IClasses) model dispatch =
        div [
            Style [
                CSSProp.FlexDirection "column"
                CSSProp.Display DisplayOptions.Flex
                CSSProp.Height "inherit"
            ]
        ] [
            yield lazyView2 Snackbar.View.view model.Snack (SnackMsg >> dispatch)
            yield
                div [
                    Style [
                        CSSProp.FlexDirection "column"
                        CSSProp.Display DisplayOptions.Flex
                        CSSProp.Height "inherit"
                    ]
                ] [            
                    div [ Style [ CSSProp.Padding (string "3em") ] ] [ 
                        match model.Waiting, model.ParseWaiting with
                        | true, false when model.ConfigDir.Directory = "" ->
                            yield circularProgress [
                                Style [CSSProp.MarginLeft "45%"]
                                DOMAttr.OnAnimationStart <| fun _ ->
                                    async {
                                        do! Async.Sleep 1000
                                        return dispatch GetDefaultDir
                                    } |> Async.StartImmediate
                            ]
                        | true, false ->
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
                        | _ -> yield content classes model dispatch
                    ]
                // Add submit or save button here
                ]
            ]

    /// Workaround for using JSS with Elmish
    /// https://github.com/mvsmal/fable-material-ui/issues/4#issuecomment-422781471
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
