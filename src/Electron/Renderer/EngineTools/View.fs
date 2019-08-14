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
        Styles.Custom("accordHead", [
            CSSProp.FontSize (theme.typography.pxToRem(15.))
            CSSProp.FlexBasis "33.33%"
            CSSProp.FlexShrink 0
        ])
        Styles.Custom("accordSubHead", [
            CSSProp.FontSize (theme.typography.pxToRem(15.))
            CSSProp.Color (theme.palette.text.secondary)
        ])
        Styles.Custom ("darkList", [
            CSSProp.BackgroundColor theme.palette.background.``default``
        ])
    ]

    let private expansionPanels (classes: IClasses) model dispatch =
        model.Panels
        |> List.map (fun p ->
                expansionPanel [
                    ExpansionPanelProp.Expanded p.Expanded
                    DOMAttr.OnChange (fun _ -> dispatch <| Expand(p))
                ] [
                    expansionPanelSummary [
                        ExpansionPanelSummaryProp.ExpandIcon <| expandMoreIcon []
                    ] [
                        typography [
                            Class classes?accordHead
                        ] [ str p.Panel.Header ]
                        typography [
                            Class classes?accordSubHead
                        ] [ str p.Panel.SubHeader ]
                    ]
                    expansionPanelDetails [] [
                    ]
                ]
            )
    let private config (classes: IClasses) model dispatch =
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
                    HTMLAttr.Label "Mordhau Engine.ini Directory"
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
                    div [ Style [ CSSProp.Padding (string "3em") ] ] 
                        <| expansionPanels classes model dispatch
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
