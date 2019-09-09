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
    open RenderUtils
    open RenderUtils.MaterialUI
    open RenderUtils.MaterialUI.Core
    open RenderUtils.MaterialUI.Themes
    open RenderUtils.MaterialUI.Props
    open MordhauBuddy.Shared.ElectronBridge

    let private styles (theme: ITheme): IStyles list =
        let drawerWidth = "240px"
        [ Styles.Root
            [ CSSProp.Display DisplayOptions.Flex
              CSSProp.Height "inherit"
              CSSProp.Custom("user-select", "none") ]
          Styles.Custom
              ("titleButton",
               [ CSSProp.Padding "5px"
                 CSSProp.PaddingRight "10px"
                 CSSProp.PaddingLeft "10px"
                 CSSProp.Color "#ffffff"
                 CSSProp.Custom("WebkitAppRegion", "no-drag")
                 CSSProp.BorderRadius "0" ])
          Styles.Custom
              ("appBar",
               [ CSSProp.ZIndex(theme.zIndex.drawer + 1)
                 CSSProp.Cursor "default"
                 CSSProp.Custom("user-select", "none")
                 CSSProp.Display DisplayOptions.Grid ])
          Styles.Custom
              ("drawer",
               [ CSSProp.Width drawerWidth
                 CSSProp.FlexShrink 0 ])
          Styles.Custom("drawerPaper", [ CSSProp.Width drawerWidth ])
          Styles.Custom
              ("content",
               [ CSSProp.FlexGrow 1
                 CSSProp.Height "inherit"
                 CSSProp.PaddingTop "9em"
                 CSSProp.PaddingLeft "2em"
                 CSSProp.PaddingRight "2em"
                 CSSProp.PaddingBottom "2em" ])
          Styles.Custom'("toolbar", theme.mixins.toolbar)
          Styles.Custom
              ("navBadge",
               [ CSSProp.Float FloatOptions.Right
                 CSSProp.Padding "1em"
                 CSSProp.MaxHeight "0em"
                 CSSProp.MarginBottom "-1em"
                 CSSProp.MarginTop "-0.25em" ]) ]

    let private pageListItem (classes: IClasses) model dispatch page =
        listItem
            [ ListItemProp.Button true
              ListItemProp.Divider(page = Community)
              HTMLAttr.Selected(model.Page = page)
              HTMLAttr.Disabled <| (match page with
                                    | MapsInstaller -> model.MapsInstaller.MapsDir.Directory = ""
                                    | FaceTools -> model.FaceTools.GameDir.Directory = ""
                                    | MordhauConfig ->
                                        model.MordhauConfig.EngineDir.Directory = ""
                                        || model.MordhauConfig.GameUserDir.Directory = ""
                                    | _ -> false
                                    |> fun b -> b || (not model.IsBridgeConnected))
              Key(pageTitle page)
              DOMAttr.OnClick(fun _ -> Navigate page |> dispatch) ]
            [ listItemText []
                  [ yield page
                          |> pageTitle
                          |> str
                    if page = MapsInstaller then
                        yield badge
                                  [ Class classes?navBadge
                                    BadgeProp.Color BadgeColor.Primary
                                    BadgeProp.Max 20
                                    BadgeProp.Invisible <| (model.MapsInstaller.UpdatesAvailable = 0 || match model.MapsInstaller.UpdateSettings with
                                                                                                        | UpdateSettings.NotifyOnly ->
                                                                                                            false
                                                                                                        | _ -> true)
                                    BadgeProp.BadgeContent <| ofInt (model.MapsInstaller.UpdatesAvailable) ] [] ] ]

    let private pageView (classes: IClasses) model dispatch =
        let allResourcesAttempted =
            model.Resources.GameConfig.AttemptedLoad && model.Resources.EngineConfig.AttemptedLoad
            && model.Resources.GameUserConfig.AttemptedLoad && model.Resources.Maps.AttemptedLoad
            && model.Resources.Community.AttemptedLoad && model.Resources.InitSetup.AttemptedLoad

        match model.Page with
        | Community ->
            let isAnyLoading =
                model.Resources.GameConfig.Loading || model.Resources.EngineConfig.Loading
                || model.Resources.GameUserConfig.Loading || model.Resources.Maps.Loading
            match model.IsBridgeConnected, allResourcesAttempted, isAnyLoading with
            | true, true, false ->
                if model.Community.LoadingElem then dispatch ResourcesLoaded
            | true, false, false ->
                match model.Resources with
                | r when r.InitSetup.AttemptedLoad |> not -> dispatch <| LoadResources(InitSetup)
                | r when r.Community.AttemptedLoad |> not -> dispatch <| LoadResources(LoadCom)
                | r when r.GameConfig.AttemptedLoad |> not -> dispatch <| LoadResources(LoadConfig(ConfigFile.Game))
                | r when r.EngineConfig.AttemptedLoad |> not -> dispatch <| LoadResources(LoadConfig(ConfigFile.Engine))
                | r when r.GameUserConfig.AttemptedLoad |> not ->
                    dispatch <| LoadResources(LoadConfig(ConfigFile.GameUserSettings))
                | r when r.Maps.AttemptedLoad |> not -> dispatch <| LoadResources(LoadMap)
                | _ -> ()
            | _ -> ()
            lazyView2 Community.View.view model.Community (CommunityMsg >> dispatch)
        | MapsInstaller -> lazyView2 MapsInstaller.View.view model.MapsInstaller (MapsInstallerMsg >> dispatch)
        | FaceTools -> lazyView2 FaceTools.View.view model.FaceTools (FaceToolsMsg >> dispatch)
        | MordhauConfig -> lazyView2 MordhauConfig.View.view model.MordhauConfig (MordhauConfigMsg >> dispatch)
        | Settings -> lazyView2 Settings.View.view model.Settings (SettingsMsg >> dispatch)
        | About -> lazyView2 About.View.view model.About (AboutMsg >> dispatch)

    let private menuView (classes: IClasses) model dispatch =
        lazyView2 ContextMenu.View.view model.ContextMenu (ContextMenuMsg >> dispatch)

    let private getTheme (m: Model) =
        let dark =
            { PaletteType = PaletteType.Dark
              PMain = "#BB86FC"
              PDark = "#3700B3"
              PCText = Some("#000")
              SMain = "#03DAC6"
              SDark = Some("#7CFDF1")
              SCText = Some("#FFF")
              EMain = "#CF6679"
              ECText = Some("#FFF")
              PaperElev2 = "#303030"
              MuiButtonCPHover = Some("#FFF")
              MuiButtonCSecondary = Some("#000") }

        let light =
            { PaletteType = PaletteType.Light
              PMain = "#6200EE"
              PDark = "#3700B3"
              PCText = None
              SMain = "#03DAC6"
              SDark = None
              SCText = None
              EMain = "#B00020"
              ECText = None
              PaperElev2 = "#FAFAFA"
              MuiButtonCPHover = None
              MuiButtonCSecondary = None }

        let createTheme themeType =
            createMuiTheme
                [ ThemeProp.Palette
                    [ PaletteProp.Type themeType.PaletteType
                      PaletteProp.Primary
                          [ yield PaletteIntentionProp.Main themeType.PMain
                            yield PaletteIntentionProp.Dark themeType.PDark
                            if themeType.PCText.IsSome then
                                yield PaletteIntentionProp.ContrastText themeType.PCText.Value ]
                      PaletteProp.Secondary
                          [ yield PaletteIntentionProp.Main themeType.SMain
                            if themeType.SDark.IsSome then yield PaletteIntentionProp.Dark themeType.SDark.Value
                            if themeType.SCText.IsSome then
                                yield PaletteIntentionProp.ContrastText themeType.SCText.Value ]
                      PaletteProp.Error
                          [ yield PaletteIntentionProp.Main themeType.EMain
                            if themeType.ECText.IsSome then
                                yield PaletteIntentionProp.ContrastText themeType.ECText.Value ]
                      PaletteProp.ContrastThreshold 3 ]
                  ThemeProp.Typography [ ThemeTypographyProp.UseNextVariants true ]
                  ThemeProp.Overrides
                      [ yield OverridesProp.MuiOutlinedInput
                                  [ Styles.Root
                                      [ CSSProp.Custom
                                          ("&$focused $notchedOutline",
                                           [ CSSProp.BorderColor themeType.SMain ] |> keyValueList CaseRules.LowerFirst)
                                        CSSProp.Custom
                                            ("&:hover $notchedOutline",
                                             [ CSSProp.BorderColor themeType.SMain ]
                                             |> keyValueList CaseRules.LowerFirst) ]
                                    Styles.NotchedOutline [ CSSProp.BorderColor themeType.SMain ] ]
                        yield OverridesProp.MuiFormLabel
                                  [ Styles.Root
                                      [ CSSProp.Custom
                                          ("&$focused",
                                           [ CSSProp.Color themeType.SMain ] |> keyValueList CaseRules.LowerFirst) ] ]
                        yield OverridesProp.MuiPaper
                                  [ Styles.Elevation2 [ CSSProp.BackgroundColor themeType.PaperElev2 ] ]
                        if themeType.MuiButtonCPHover.IsSome && themeType.MuiButtonCSecondary.IsSome then
                            yield OverridesProp.MuiButton
                                      [ Styles.Root
                                          [ CSSProp.TransitionProperty "background-color, color, box-shadow, border" ]
                                        Styles.ContainedPrimary
                                            [ CSSProp.Custom
                                                ("&:hover",
                                                 [ CSSProp.Color themeType.MuiButtonCPHover.Value ]
                                                 |> keyValueList CaseRules.LowerFirst) ]
                                        Styles.ContainedSecondary [ CSSProp.Color themeType.MuiButtonCSecondary.Value ] ]
                        yield OverridesProp.MuiCard [ Styles.Root [ CSSProp.Overflow OverflowOptions.Visible ] ]
                        yield OverridesProp.MuiStepper [ Styles.Root [ CSSProp.BorderRadius "4px" ] ]
                        yield OverridesProp.MuiExpansionPanel
                                  [ Styles.Root
                                      [ CSSProp.BorderBottom "1px solid"
                                        CSSProp.BorderColor "#BB86FC !important"
                                        CSSProp.BoxShadow "none"
                                        CSSProp.Custom
                                            ("&:last-child",
                                             [ CSSProp.BorderBottom "0em" ] |> keyValueList CaseRules.LowerFirst)
                                        CSSProp.Custom
                                            ("&:before",
                                             [ CSSProp.Display DisplayOptions.None ]
                                             |> keyValueList CaseRules.LowerFirst)
                                        CSSProp.Custom
                                            ("&$expanded",
                                             [ CSSProp.Margin "auto" ] |> keyValueList CaseRules.LowerFirst) ]
                                    Styles.Rounded
                                        [ CSSProp.Custom
                                            ("&:last-child",
                                             [ CSSProp.BorderBottom "0em" ] |> keyValueList CaseRules.LowerFirst) ] ]
                        yield OverridesProp.MuiExpansionPanelSummary
                                  [ Styles.Root
                                      [ CSSProp.BorderBottom "1px solid"
                                        CSSProp.BorderColor "#BB86FC !important" ]
                                    Styles.Content
                                        [ CSSProp.Custom
                                            ("&$expanded",
                                             [ CSSProp.Margin "1em 0em" ] |> keyValueList CaseRules.LowerFirst) ]
                                    Styles.ExpandIcon [ CSSProp.ZIndex "2" ] ]
                        yield OverridesProp.MuiExpansionPanelDetails [ Styles.Root [ CSSProp.Padding("0em") ] ]
                        yield OverridesProp.MuiSkeleton
                                  [ Styles.Text
                                      [ CSSProp.MarginTop "0em"
                                        CSSProp.MarginBottom "0em" ] ]
                        yield OverridesProp.MuiFab [ Styles.Secondary [ CSSProp.Color "#000" ] ]
                        yield OverridesProp.MuiTableCell [ Styles.Root [ CSSProp.Padding "1em" ] ]
                        yield OverridesProp.MuiBadge
                                  [ Styles.Badge
                                      [ CSSProp.Position PositionOptions.Inherit
                                        CSSProp.MarginLeft "-1em" ] ] ] ]
            |> ProviderTheme.Theme

        if m.Store.DarkTheme then createTheme dark
        else createTheme light

    let private view' (classes: IClasses) model dispatch =
        let hideIfMax (b: bool) =
            match model.IsMax = b with
            | true -> Display DisplayOptions.None
            | false -> Display DisplayOptions.Flex

        muiThemeProvider [ Theme <| getTheme (model) ]
            [ div
                [ Class classes?root
                  DOMAttr.OnContextMenu(fun e ->
                      async {
                          e.preventDefault()
                          e
                          |> ContextMenu.Types.Msg.Open
                          |> ContextMenuMsg
                          |> dispatch
                      }
                      |> Async.StartImmediate) ]
                  [ cssBaseline []
                    appBar
                        [ Class classes?appBar
                          AppBarProp.Position AppBarPosition.Fixed
                          Style
                              [ CSSProp.BackgroundColor
                                  (if model.Store.DarkTheme then "#424242"
                                   else "#6200EE") ] ]
                        [ toolbar
                            [ Style
                                [ CSSProp.Padding "0px"
                                  CSSProp.BackgroundColor
                                      (if model.Store.DarkTheme then "#212121"
                                       else "#3700B3")
                                  CSSProp.MinHeight "0px"
                                  CSSProp.Custom("WebkitAppRegion", "drag") ] ]
                              [ div
                                  [ Style
                                      [ CSSProp.Width "93%"
                                        CSSProp.Padding "5px"
                                        CSSProp.Display DisplayOptions.InlineFlex ] ]
                                    [ img
                                        [ HTMLAttr.Src(stat "MB_SquareFullRes.png")
                                          HTMLAttr.Width "25px"
                                          HTMLAttr.Height "25px"
                                          Style
                                              [ CSSProp.BorderRadius "4px"
                                                CSSProp.MarginRight "1em" ] ]
                                      typography
                                          [ TypographyProp.Variant TypographyVariant.Subtitle2
                                            Style
                                                [ CSSProp.Width "93%"
                                                  CSSProp.PaddingTop "2px"
                                                  CSSProp.Color "#ffffff" ] ]
                                          [ sprintf "%s - %s" Info.name Info.version |> str ] ]
                                iconButton
                                    [ Class classes?titleButton
                                      DOMAttr.OnClick <| fun _ -> window.minimize() ] [ windowMinimizeIcon [] ]
                                iconButton
                                    [ Class classes?titleButton
                                      DOMAttr.OnClick <| fun _ ->
                                        window.maximize()
                                        true
                                        |> MinMaxMsg
                                        |> dispatch
                                      Style [ hideIfMax true ] ] [ windowMaximizeIcon [] ]
                                iconButton
                                    [ Class classes?titleButton
                                      DOMAttr.OnClick <| fun _ ->
                                        window.unmaximize()
                                        false
                                        |> MinMaxMsg
                                        |> dispatch
                                      Style [ hideIfMax false ] ] [ windowRestoreIcon [] ]
                                iconButton
                                    [ Class classes?titleButton
                                      DOMAttr.OnClick <| fun _ -> window.hide() ] [ windowCloseIcon [] ] ]
                          toolbar [ Style [ CSSProp.PaddingRight "0" ] ]
                              [ div [ Style [ CSSProp.Width "100%" ] ]
                                    [ img
                                        [ HTMLAttr.Src(stat "MB_HorizFullRes.png")
                                          HTMLAttr.Width "auto"
                                          HTMLAttr.Height "70px"
                                          Style
                                              [ CSSProp.BorderRadius "4px"
                                                CSSProp.Display DisplayOptions.Block
                                                CSSProp.Padding "0.2em 2em" ] ] ]
                                iconButton
                                    [ Class classes?titleButton
                                      DOMAttr.OnClick <| fun _ -> dispatch StartPatch
                                      Style
                                          [ CSSProp.Display <|
                                                if model.UpdatePending.Ready then
                                                    DisplayOptions.Inherit
                                                else DisplayOptions.None
                                            CSSProp.Color "#ffffff"
                                            CSSProp.BorderRadius "20%" ] ] [ downloadIcon [] ]
                                iconButton
                                    [ Class classes?titleButton
                                      DOMAttr.OnClick <| fun _ ->
                                        Store.Msg.ToggleDarkTheme
                                        |> StoreMsg
                                        |> dispatch
                                      Style
                                          [ CSSProp.Color "#ffffff"
                                            CSSProp.BorderRadius "20%" ] ] [ themeLightDarkIcon [] ] ] ]
                    drawer
                        [ Class classes?drawer
                          DrawerProp.Variant DrawerVariant.Permanent
                          Classes [ ClassNames.Paper classes?drawerPaper ] ]
                        [ list
                            [ Component(ReactElementType.ofHtmlElement "nav")
                              Style [ CSSProp.PaddingTop "108px" ] ]
                              [ Page.All
                                |> List.map (pageListItem classes model dispatch)
                                |> ofList ] ]
                    main [ Class classes?content ]
                        [ pageView classes model dispatch
                          menuView classes model dispatch ] ] ]

    /// Workaround for using JSS with Elmish
    /// https://github.com/mvsmal/fable-material-ui/issues/4#issuecomment-423477900
    type private IProps =
        abstract model: Model with get, set
        abstract dispatch: (Msg -> unit) with get, set
        inherit IClassesProps

    type private Component(p) =
        inherit PureStatelessComponent<IProps>(p)
        let viewFun (p: IProps) = view' p.classes p.model p.dispatch
        let viewWithStyles = withStyles (StyleType.Func styles) [] viewFun
        override this.render() = ReactElementType.create viewWithStyles this.props []

    let view (model: Model) (dispatch: Msg -> unit): ReactElement =
        let props =
            jsOptions<IProps> (fun p ->
                p.model <- model
                p.dispatch <- dispatch)
        ofType<Component, _, _> props []
