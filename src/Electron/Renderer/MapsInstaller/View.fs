namespace MordhauBuddy.App.MapsInstaller

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

    let private tabContent (classes: IClasses) model dispatch =
        match model.TabSelected with
        | Available ->
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
                //img [
                //    HTMLAttr.Hidden (model.ImgLoaded |> not)
                //    DOMAttr.OnLoad(fun _ -> dispatch ImgSkeleton)
                //    HTMLAttr.Src (stat "frankenstein.png")
                //    Style [ 
                //        CSSProp.BorderRadius "4px"
                //    ]
                //]
                //skeleton [ 
                //    HTMLAttr.Hidden <| model.ImgLoaded
                //    HTMLAttr.Width "229px"
                //    HTMLAttr.Height "308px"
                //    SkeletonProp.DisableAnimate true 
                //]
            ]
        | Installed -> div [] []
        | Installing -> div [] []

    let private view' (classes: IClasses) model dispatch =
        div [
            Style [
                CSSProp.FlexDirection "column"
                CSSProp.Display DisplayOptions.Flex
                CSSProp.Height "inherit"
                CSSProp.Padding "2em"
            ]
        ] [
            yield lazyView2 Snackbar.View.view model.Snack (SnackMsg >> dispatch)
            yield
                card [ CardProp.Raised true ] [
                    tabs [
                        HTMLAttr.Value (model.TabSelected.GetTag)
                        TabsProp.Variant TabsVariant.FullWidth
                        TabsProp.ScrollButtons ScrollButtonsType.On
                        TabsProp.IndicatorColor TabsIndicatorColor.Secondary
                        TabsProp.TextColor TabsTextColor.Secondary
                        TabsProp.Centered true
                        TabsProp.OnChange (fun _ tabPicked -> dispatch <| TabSelected(Tab.GetTabFromTag(tabPicked)) )
                    ] <|
                        (Tab.GetTabs
                        |> Seq.map (fun t ->
                            tab [ HTMLAttr.Label (t.Text)]))
                    divider []
                    div [
                        Style [ CSSProp.Padding "2em"; CSSProp.MinHeight "5em" ]
                    ] [ 
                        tabContent classes model dispatch
                    ]
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
