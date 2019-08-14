namespace MordhauBuddy.App

module View =
    open Elmish.React
    open Fable.Core
    open Fable.Core.JsInterop
    open Fable.React
    open Fable.React.Props
    open Fable.MaterialUI
    open Fable.MaterialUI.Core
    open Fable.MaterialUI.MaterialDesignIcons
    open Bindings
    open FSharp.Core /// To avoid shadowing Result<_,_>
    open Types
    open State

    let private styles (theme : ITheme) : IStyles list =
        let drawerWidth = "240px"
        [
            Styles.Root [
                CSSProp.Display DisplayOptions.Flex
                CSSProp.Height "inherit"
                CSSProp.Custom("user-select","none")
            ]
            Styles.Custom ("titleButton", [
                CSSProp.Padding "5px"
                CSSProp.PaddingRight "10px"
                CSSProp.PaddingLeft "10px"
                CSSProp.Color "#ffffff"
                CSSProp.Custom ("WebkitAppRegion", "no-drag")
                CSSProp.BorderRadius "0"
            ])
            Styles.Custom ("appBar", [
                CSSProp.ZIndex (theme.zIndex.drawer + 1)
                CSSProp.Cursor "default"
                CSSProp.Custom("user-select", "none")
                CSSProp.Display DisplayOptions.Grid
            ])
            Styles.Custom ("drawer", [
                CSSProp.Width drawerWidth
                CSSProp.FlexShrink 0
            ])
            Styles.Custom ("drawerPaper", [
                CSSProp.Width drawerWidth
            ])
            Styles.Custom ("content", [
                CSSProp.FlexGrow 1
                CSSProp.Height "inherit"
                CSSProp.PaddingTop "9em"
                CSSProp.PaddingLeft "2em"
                CSSProp.PaddingRight "2em"
                CSSProp.PaddingBottom "2em"
            ])
            Styles.Custom' ("toolbar", theme.mixins.toolbar)
        ]

    let private pageListItem model dispatch page =
        listItem [
            ListItemProp.Button true
            ListItemProp.Divider (page = Home)
            HTMLAttr.Selected (model.Page = page)
            HTMLAttr.Disabled (not model.IsBridgeConnected)
            Key (pageTitle page)
            DOMAttr.OnClick (fun _ -> Navigate page |> dispatch)
        ] [ listItemText [ ] [ page |> pageTitle |> str ] ]

    let private pageView model dispatch =
        match model.Page with
        | Home ->
            if model.IsBridgeConnected then
                typography [] 
                    [ str "This app contains simple demos showing how certain Material-UI components can be used with Elmish." ]
            else 
                div [ Style [ CSSProp.Padding "10em" ] ] [
                    typography [
                        TypographyProp.Variant TypographyVariant.H6
                        TypographyProp.Align TypographyAlign.Center
                        Style [ CSSProp.PaddingBottom "10em" ]
                    ] [
                        str "Preparing for battle..."
                    ]
                    circularProgress [
                        CircularProgressProp.Size <| CircularProgressSize.Case2 "5em"
                        Style [ CSSProp.MarginLeft "45%" ]
                    ] 
                ]
        | FaceTools -> lazyView2 FaceTools.View.view model.FaceTools (FaceToolsMsg >> dispatch)
        | EngineTools -> lazyView2 EngineTools.View.view model.EngineTools (EngineToolsMsg >> dispatch)
        | Settings -> lazyView2 Settings.View.view model.Settings (SettingsMsg >> dispatch)
        | About -> lazyView2 About.View.view model.About (AboutMsg >> dispatch)

    let private menuView model dispatch =
        lazyView2 ContextMenu.View.view model.ContextMenu (ContextMenuMsg >> dispatch)

    let private getTheme (m: Model) =
        if m.Store.DarkTheme then 
            createMuiTheme [
                ThemeProp.Palette [
                    PaletteProp.Type PaletteType.Dark
                    PaletteProp.Primary [
                        PaletteIntentionProp.Main "#BB86FC"
                        PaletteIntentionProp.Dark "#3700B3"
                        PaletteIntentionProp.ContrastText "#000"
                    ]
                    PaletteProp.Secondary [
                        PaletteIntentionProp.Main "#03DAC6"
                        PaletteIntentionProp.Dark "#7CFDF1"
                        PaletteIntentionProp.ContrastText "#FFF"
                    ]
                    PaletteProp.Error [
                        PaletteIntentionProp.Main "#CF6679"
                        PaletteIntentionProp.ContrastText "#FFF"
                    ]
                    PaletteProp.ContrastThreshold 3
                ]
                ThemeProp.Typography [
                    ThemeTypographyProp.UseNextVariants true
                ]
                ThemeProp.Overrides [
                    OverridesProp.MuiOutlinedInput [
                        Styles.Root [
                            CSSProp.Custom ("&$focused $notchedOutline", [
                                CSSProp.BorderColor "#03DAC6"
                            ] |> keyValueList CaseRules.LowerFirst)
                            CSSProp.Custom ("&:hover $notchedOutline", [
                                CSSProp.BorderColor "#03DAC6"
                            ] |> keyValueList CaseRules.LowerFirst)
                        ]
                        Styles.NotchedOutline [
                            CSSProp.BorderColor "#03DAC6"
                        ]
                    ]
                    OverridesProp.MuiFormLabel [
                        Styles.Root [
                            CSSProp.Custom ("&$focused", [
                                CSSProp.Color "#03DAC6"
                            ] |> keyValueList CaseRules.LowerFirst)
                        ]
                    ]
                    OverridesProp.MuiPaper [
                        Styles.Elevation2 [
                            CSSProp.BackgroundColor "#303030"
                        ]
                    ]
                    OverridesProp.MuiButton [
                        Styles.Root [
                            CSSProp.TransitionProperty "background-color, color, box-shadow, border"
                        ]
                        Styles.ContainedPrimary [
                            CSSProp.Custom ("&:hover", [
                                CSSProp.Color "#FFF"
                            ] |> keyValueList CaseRules.LowerFirst)
                        ]
                        Styles.ContainedSecondary [
                            CSSProp.Color "#000"
                        ]
                    ]
                    OverridesProp.MuiStepper [
                        Styles.Root [
                            CSSProp.BorderRadius "4px"
                        ]
                    ]
                    OverridesProp.MuiExpansionPanel [
                        Styles.Root [
                            CSSProp.BorderBottom "1px solid"
                            CSSProp.BorderColor "#BB86FC !important"
                            CSSProp.BoxShadow "none"
                            CSSProp.Custom ("&:not(:last-child)", [
                                CSSProp.BorderBottom "0em"  
                            ] |> keyValueList CaseRules.LowerFirst)
                            CSSProp.Custom ("&:before", [
                                CSSProp.Display DisplayOptions.None
                            ] |> keyValueList CaseRules.LowerFirst)
                            CSSProp.Custom ("&$expanded", [
                                CSSProp.Margin "auto"
                            ] |> keyValueList CaseRules.LowerFirst)
                        ]
                        Styles.Rounded [
                            CSSProp.Custom ("&:last-child", [
                                CSSProp.BorderBottom "0em"  
                            ] |> keyValueList CaseRules.LowerFirst)
                        ]
                    ]
                    OverridesProp.MuiExpansionPanelSummary [
                        Styles.Root [
                            CSSProp.BorderBottom "1px solid"
                            CSSProp.BorderColor "#BB86FC !important"
                        ]
                        Styles.Content [
                            CSSProp.Custom ("&$expanded", [
                                CSSProp.Margin "1em 0em"
                            ] |> keyValueList CaseRules.LowerFirst)
                        ]
                    ]
                    OverridesProp.MuiExpansionPanelDetails [
                        Styles.Root [
                            CSSProp.Padding ("2em")
                        ]
                    ]
                ]
            ]
            |> ProviderTheme.Theme
        else
            createMuiTheme [
                ThemeProp.Palette [
                    PaletteProp.Type PaletteType.Light
                    PaletteProp.Primary [
                        PaletteIntentionProp.Main "#6200EE"
                        PaletteIntentionProp.Dark "#3700B3"
                    ]
                    PaletteProp.Secondary [
                        PaletteIntentionProp.Main "#03DAC6"
                    ]
                    PaletteProp.Error [
                        PaletteIntentionProp.Main "#B00020"
                    ]
                    PaletteProp.Action [
                        PaletteActionProp.HoverOpacity 0.2
                    ]
                ]
                ThemeProp.Typography [
                    ThemeTypographyProp.UseNextVariants true
                ]
                ThemeProp.Overrides [
                    OverridesProp.MuiOutlinedInput [
                        Styles.Root [
                            CSSProp.Custom ("&$focused $notchedOutline", [
                                CSSProp.BorderColor "#03DAC6"
                            ] |> keyValueList CaseRules.LowerFirst)
                            CSSProp.Custom ("&:hover $notchedOutline", [
                                CSSProp.BorderColor "#03DAC6"
                            ] |> keyValueList CaseRules.LowerFirst)
                        ]
                        Styles.NotchedOutline [
                            CSSProp.BorderColor "#03DAC6"
                        ]
                    ]
                    OverridesProp.MuiFormLabel [
                        Styles.Root [
                            CSSProp.Custom ("&$focused", [
                                CSSProp.Color "#03DAC6"
                            ] |> keyValueList CaseRules.LowerFirst)
                        ]
                    ]
                    OverridesProp.MuiPaper [
                        Styles.Elevation2 [
                            CSSProp.BackgroundColor "#FAFAFA"
                        ]
                    ]
                    OverridesProp.MuiStepper [
                        Styles.Root [
                            CSSProp.BorderRadius "4px"
                        ]
                    ]
                    OverridesProp.MuiExpansionPanel [
                        Styles.Root [
                            CSSProp.BorderBottom "1px solid"
                            CSSProp.BorderColor "#BB86FC !important"
                            CSSProp.BoxShadow "none"
                            CSSProp.Custom ("&:not(:last-child)", [
                                CSSProp.BorderBottom "0em"  
                            ] |> keyValueList CaseRules.LowerFirst)
                            CSSProp.Custom ("&:before", [
                                CSSProp.Display DisplayOptions.None
                            ] |> keyValueList CaseRules.LowerFirst)
                            CSSProp.Custom ("&$expanded", [
                                CSSProp.Margin "auto"
                            ] |> keyValueList CaseRules.LowerFirst)
                        ]
                        Styles.Rounded [
                            CSSProp.Custom ("&:last-child", [
                                CSSProp.BorderBottom "0em"  
                            ] |> keyValueList CaseRules.LowerFirst)
                        ]
                    ]
                    OverridesProp.MuiExpansionPanelSummary [
                        Styles.Root [
                            CSSProp.BorderBottom "1px solid"
                            CSSProp.BorderColor "#6200EE !important"
                        ]
                        Styles.Content [
                            CSSProp.Custom ("&$expanded", [
                                CSSProp.Margin "1em 0em"
                            ] |> keyValueList CaseRules.LowerFirst)
                        ]
                    ]
                    OverridesProp.MuiExpansionPanelDetails [
                        Styles.Root [
                            CSSProp.Padding ("2em")
                        ]
                    ]
                ]
            ]
            |> ProviderTheme.Theme

    let private view' (classes : IClasses) model dispatch =
        let hideIfMax (b: bool) =
            match window.isMaximized() = b with
            | true -> Display DisplayOptions.None
            | false -> Display DisplayOptions.Flex

        muiThemeProvider [Theme <| getTheme(model)] [
            div [
                Class classes?root
                DOMAttr.OnContextMenu (fun e ->
                    async {
                        e.preventDefault()
                        e |> ContextMenu.Types.Msg.Open |> ContextMenuMsg |> dispatch
                    } |> Async.StartImmediate)
            ] [
                cssBaseline []
                appBar [
                    Class classes?appBar
                    AppBarProp.Position AppBarPosition.Fixed
                    Style [
                        CSSProp.BackgroundColor (if model.Store.DarkTheme then "#424242" else "#6200EE")
                    ]
                ] [
                    toolbar [
                        Style [
                            CSSProp.Padding "0px"
                            CSSProp.BackgroundColor (if model.Store.DarkTheme then "#212121" else "#3700B3")
                            CSSProp.MinHeight "0px"
                            CSSProp.Custom("WebkitAppRegion", "drag")
                        ]
                    ] [
                        typography [
                            TypographyProp.Variant TypographyVariant.Subtitle2
                            Style [
                                CSSProp.Width "93%"
                                CSSProp.Padding "5px"
                                CSSProp.Color "#ffffff"
                            ]
                        ] [ sprintf "%s - %s" Info.name Info.version |> str ]
                        iconButton [
                            DOMAttr.OnClick (fun _ -> window.minimize())
                            Class classes?titleButton
                        ] [ windowMinimizeIcon [] ]
                        iconButton [
                            DOMAttr.OnClick (fun _ ->
                                window.maximize()
                                true |> MinMaxMsg |> dispatch)
                            Class classes?titleButton
                            Style [hideIfMax true]
                        ] [ windowMaximizeIcon [] ]
                        iconButton [
                            DOMAttr.OnClick (fun _ -> 
                                window.unmaximize()
                                false |> MinMaxMsg |> dispatch)
                            Class classes?titleButton
                            Style [hideIfMax false]
                        ] [ windowRestoreIcon [] ]
                        iconButton [
                            DOMAttr.OnClick (fun _ -> window.close())
                            Class classes?titleButton
                        ] [ windowCloseIcon [] ]
                    ]
                    toolbar [ Style [ CSSProp.PaddingRight "0" ] ] [
                        typography [
                            TypographyProp.Variant TypographyVariant.H6
                            Style [
                                CSSProp.Width "100%"
                                CSSProp.Color "#ffffff"
                            ]
                        ] [ model.Page |> pageTitle |> str ]
                        iconButton [
                            DOMAttr.OnClick (fun _ -> 
                                Store.Msg.ToggleDarkTheme |> StoreMsg |> dispatch)
                            Class classes?titleButton
                            Style [ CSSProp.Color "#ffffff"; CSSProp.BorderRadius "20%" ]
                        ] [ themeLightDarkIcon [] ]
                    ]
                ]
                drawer [
                    Class classes?drawer
                    DrawerProp.Variant DrawerVariant.Permanent
                    Classes [ ClassNames.Paper classes?drawerPaper ]
                ] [
                    list [ 
                        Component (ReactElementType.ofHtmlElement "nav")
                        Style [ CSSProp.PaddingTop "108px" ]
                    ] [ Page.All |> List.map (pageListItem model dispatch) |> ofList ]
                ]
                main [ Class classes?content ] [ 
                    pageView model dispatch
                    menuView model dispatch 
                ]
            ]
        ]

    /// Workaround for using JSS with Elmish
    /// https://github.com/mvsmal/fable-material-ui/issues/4#issuecomment-423477900
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
