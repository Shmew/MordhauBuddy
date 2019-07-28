namespace MordhauBuddy.App

module App =
    open System
    open Elmish
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
    open Utils

    type Page =
        | Home
        | AutoComplete
        | Badges
        | Dialogs
        | SaveLoad
        | Selects
        | Snackbars
        | StaticAssets
        | TextFields
        | INITest
        static member All =
            [ Home; AutoComplete; Badges; Dialogs; SaveLoad; Selects; Snackbars; StaticAssets; TextFields; INITest ]

    let pageTitle =
        function
        | Home -> "Home"
        | AutoComplete -> "Autocomplete"
        | Badges -> "Badges"
        | Dialogs -> "Dialogs"
        | SaveLoad -> "Save/load"
        | Selects -> "Selects"
        | StaticAssets -> "Static assets"
        | Snackbars -> "Snackbars"
        | TextFields -> "Text fields"
        | INITest -> "INI Test"

    type Msg =
        | Navigate of Page
        | MinMaxMsg of bool
        | DarkTheme of bool
        | AutoCompleteMsg of AutoComplete.Msg
        | BadgesMsg of Badges.Msg
        | DialogsMsg of Dialogs.Msg
        | SaveLoadMsg of SaveLoad.Msg
        | SelectsMsg of Selects.Msg
        | SnackbarsMsg of Snackbars.Msg
        | TextFieldsMsg of TextFields.Msg
        | INITestMsg of INITest.Msg

    type Model =
        { Page : Page
          IsMax : bool
          IsDarkTheme : bool
          AutoCompleteDownshift : AutoComplete.Model
          Badges : Badges.Model
          Dialogs : Dialogs.Model
          SaveLoad : SaveLoad.Model
          Selects : Selects.Model
          Snackbars : Snackbars.Model
          TextFields : TextFields.Model
          INITest : INITest.Model }

    let private window = getRemoteWin()

    let init() =
        let m =
            { Page = Home
              IsMax = window.isMaximized() 
              IsDarkTheme = false //use store to get this later
              AutoCompleteDownshift = AutoComplete.init()
              Badges = Badges.init()
              Dialogs = Dialogs.init()
              SaveLoad = SaveLoad.init()
              Selects = Selects.init()
              Snackbars = Snackbars.init()
              TextFields = TextFields.init()
              INITest = INITest.init() } 
        m, Cmd.none

    let update msg m =
        match msg with
        | Navigate p ->
            { m with Page = p }, Cmd.none
        | MinMaxMsg b ->
            { m with IsMax = b }, Cmd.none
        | DarkTheme b ->
            { m with IsDarkTheme = b }, Cmd.none
        | AutoCompleteMsg msg' ->
            { m with AutoCompleteDownshift = AutoComplete.update msg' m.AutoCompleteDownshift }, Cmd.none
        | BadgesMsg msg' ->
            { m with Badges = Badges.update msg' m.Badges }, Cmd.none
        | DialogsMsg msg' ->
            { m with Dialogs = Dialogs.update msg' m.Dialogs }, Cmd.none
        | SaveLoadMsg msg' ->
            let m', cmd = SaveLoad.update msg' m.SaveLoad
            { m with SaveLoad = m' }, Cmd.map SaveLoadMsg cmd
        | SelectsMsg msg' ->
            { m with Selects = Selects.update msg' m.Selects }, Cmd.none
        | SnackbarsMsg msg' ->
            let m', cmd = Snackbars.update msg' m.Snackbars
            { m with Snackbars = m' }, Cmd.map SnackbarsMsg cmd
        | TextFieldsMsg msg' ->
            { m with TextFields = TextFields.update msg' m.TextFields }, Cmd.none
        | INITestMsg msg' ->
            { m with INITest = INITest.update msg' m.INITest }, Cmd.none

    // Domain/Elmish above, view below
    let private styles (theme : ITheme) : IStyles list =
        let drawerWidth = "240px"
        [
            Styles.Root [
                Display DisplayOptions.Flex
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
                Width drawerWidth
                FlexShrink 0
            ])
            Styles.Custom ("drawerPaper", [
                Width drawerWidth
            ])
            Styles.Custom ("content", [
                FlexGrow 1
                CSSProp.Padding (theme.spacing.unit * 3)
            ])
            Styles.Custom' ("toolbar", theme.mixins.toolbar)
        ]

    let private pageListItem model dispatch page =
        listItem [
            ListItemProp.Button true
            ListItemProp.Divider (page = Home)
            HTMLAttr.Selected (model.Page = page)
            Key (pageTitle page)
            DOMAttr.OnClick (fun _ -> Navigate page |> dispatch)
        ] [
            listItemText [ ] [ page |> pageTitle |> str ]
        ]

    let private pageView model dispatch =
        match model.Page with
        | Home ->
            typography []
                [ str
                      "This app contains simple demos showing how certain Material-UI components can be used with Elmish." ]
        | AutoComplete -> lazyView2 AutoComplete.view model.AutoCompleteDownshift (AutoCompleteMsg >> dispatch)
        | Badges -> lazyView2 Badges.view model.Badges (BadgesMsg >> dispatch)
        | Dialogs -> lazyView2 Dialogs.view model.Dialogs (DialogsMsg >> dispatch)
        | SaveLoad -> lazyView2 SaveLoad.view model.SaveLoad (SaveLoadMsg >> dispatch)
        | Selects -> lazyView2 Selects.view model.Selects (SelectsMsg >> dispatch)
        | Snackbars -> lazyView2 Snackbars.view model.Snackbars (SnackbarsMsg >> dispatch)
        | StaticAssets ->
            div [] [
                typography [ Paragraph true ] [ str "This demo shows how to use static assets such as images." ]
                avatar [ Src (stat "avatar.jpg") ] []
            ]
        | TextFields -> lazyView2 TextFields.view model.TextFields (TextFieldsMsg >> dispatch)
        | INITest -> lazyView2 INITest.view model.INITest (INITestMsg >> dispatch)

    let private getTheme (m: Model) =
        if m.IsDarkTheme then 
            createMuiTheme [
                ThemeProp.Palette [
                    PaletteProp.Type PaletteType.Dark
                    PaletteProp.Primary [
                        PaletteIntentionProp.Main "#BB86FC"
                        PaletteIntentionProp.Dark "#3700B3"
                    ]
                    PaletteProp.Secondary [
                        PaletteIntentionProp.Main "#03DAC6"
                    ]
                    PaletteProp.Error [
                        PaletteIntentionProp.Main "#CF6679"
                    ]
                ]
                ThemeProp.Typography [
                    ThemeTypographyProp.UseNextVariants true
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
            ] [ 
                cssBaseline []
                appBar [
                    Class classes?appBar
                    AppBarProp.Position AppBarPosition.Fixed
                    Style [
                        CSSProp.BackgroundColor (if model.IsDarkTheme then "#424242" else "#6200EE")
                    ]
                ] [
                toolbar [
                    Style [
                        CSSProp.Padding "0px"
                        CSSProp.BackgroundColor (if model.IsDarkTheme then "#212121" else "#3700B3")
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
                    ] [
                        windowMinimizeIcon []
                    ]
                    iconButton [
                        DOMAttr.OnClick (fun _ ->
                            window.maximize()
                            true |> MinMaxMsg |> dispatch)
                        Class classes?titleButton
                        Style [hideIfMax true]
                    ] [
                        windowMaximizeIcon []
                    ]
                    iconButton [
                        DOMAttr.OnClick (fun _ -> 
                            window.unmaximize()
                            false |> MinMaxMsg |> dispatch)
                        Class classes?titleButton
                        Style [hideIfMax false]
                    ] [
                        windowRestoreIcon []
                    ]
                    iconButton [
                        DOMAttr.OnClick (fun _ -> window.close())
                        Class classes?titleButton
                    ] [
                        windowCloseIcon []
                    ]
                ]
                toolbar [ 
                    Style [CSSProp.PaddingRight "0"]
                ] [
                    typography [
                        TypographyProp.Variant TypographyVariant.H6
                        
                        Style [
                            CSSProp.Width "100%"
                            CSSProp.Color "#ffffff"
                        ]
                    ] [ model.Page |> pageTitle |> str ]
                    iconButton [
                        DOMAttr.OnClick (fun _ -> 
                            model.IsDarkTheme |> not 
                            |> DarkTheme |> dispatch)
                        Class classes?titleButton
                        Style [CSSProp.Color "#ffffff"; CSSProp.BorderRadius "20%"]
                    ] [
                        themeLightDarkIcon []
                    ]
                    ]
                ]
                drawer [
                    Class classes?drawer
                    DrawerProp.Variant DrawerVariant.Permanent
                    Classes [ ClassNames.Paper classes?drawerPaper ]
                ] [
                    list [ 
                        Component (ReactElementType.ofHtmlElement "nav")
                        Style [CSSProp.PaddingTop "108px"]
                    ] [
                        Page.All |> List.map (pageListItem model dispatch) |> ofList
                    ]
                ]
                main [ 
                    Class classes?content
                    Style [CSSProp.PaddingTop "108px"]
                ] [
                    div [ Class classes?toolbar ] []
                    pageView model dispatch
                ]
            ]
        ]
    
    // Workaround for using JSS with Elmish
    // https://github.com/mvsmal/fable-material-ui/issues/4#issuecomment-423477900
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
