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
    open RenderUtils.String
    open RenderUtils.Validation
    open RenderUtils.MaterialUI
    open RenderUtils.MaterialUI.Core
    open Elmish.React
    open Electron
    open Types

    let private styles (theme : ITheme) : IStyles list = [
        Styles.Custom ("darkList", [
            CSSProp.BackgroundColor theme.palette.background.``default``
        ])
    ]

    let private renderAvailableMaps (classes: IClasses) model dispatch =
        let getName (map : MapTypes.CommunityMap) =
            match map.Name with
            | Some(name) -> name
            | None -> map.Folder

        model.Available
        |> List.map (fun map ->
            grid [
                GridProp.Item true
            ] [
                card [
                    MaterialProp.Elevation 2
                    CardProp.Raised true 
                    Style [
                        CSSProp.MarginBottom "2em"
                        CSSProp.MarginLeft "1em"
                        CSSProp.MarginRight "1em"
                        CSSProp.Width "50em"
                    ]
                ] [
                    cardHeader [
                        CardHeaderProp.Title <| str (map |> getName)
                        CardHeaderProp.Subheader <| str (map.Version.GetString())
                        CardHeaderProp.Action <|
                            fab [
                                MaterialProp.Color ComponentColor.Secondary
                                FabProp.Size FabSize.Medium
                                DOMAttr.OnClick <| fun _ -> dispatch (Install(map.GetName(), map.Folder))
                            ] [ addIcon [] ]
                    ] []
                    cardMedia [
                        match map.Image with
                        | Some(mapImage) ->
                            yield CardMediaProp.Image mapImage
                            yield HTMLAttr.Title <| getName map
                        | _ ->
                            yield HTMLAttr.Title "No picture provided"
                        yield 
                            Style [
                                CSSProp.Width "50em"
                                CSSProp.Height "20em"
                            ]
                    ]
                    cardContent [
                    ] <|
                        (map.GetMetaData() 
                        |> List.map (fun s ->
                            typography [
                                TypographyProp.Variant TypographyVariant.Body2
                            ] [ str s ] ))
                ]
            ])

    let private renderInstalledMaps (classes: IClasses) model dispatch =
        let getName (map : MapTypes.CommunityMap) =
            match map.Name with
            | Some(name) -> name
            | None -> map.Folder

        model.Installed
        |> List.map (fun map -> 
            tableRow [] [
                tableCell [
                    TableCellProp.Align TableCellAlign.Left
                ] [ str (map |> getName) ]
                tableCell [
                    TableCellProp.Align TableCellAlign.Right
                ] [ map.Author |> defStr ]
                tableCell [
                    TableCellProp.Align TableCellAlign.Right
                ] [ map.GetDate() |> defStr ]
                tableCell [
                    TableCellProp.Align TableCellAlign.Right
                ] [ str <| map.Version.GetString() ]
                tableCell [
                    TableCellProp.Align TableCellAlign.Right
                ] [ 
                    map.FileSize 
                    |> Option.map (fun v -> 
                        sprintf "%s %s" (v |> string) "MB") 
                    |> defStr 
                ]
                tableCell [
                    TableCellProp.Align TableCellAlign.Right
                ] [ map.GetPlayers() |> defStr ]
            ])

    let private renderInstallingMaps (classes: IClasses) model dispatch =
        let getName (map : MapTypes.CommunityMap) =
            match map.Name with
            | Some(name) -> name
            | None -> map.Folder

        model.Installing
        |> List.map (fun map ->
            tableRow [] [
                tableCell [
                    TableCellProp.Align TableCellAlign.Left
                ] [ str (map.Map |> getName) ]
                tableCell [
                    TableCellProp.Align TableCellAlign.Center
                    Style [ CSSProp.MinWidth "15em" ]
                ] [ 
                    linearProgress [ LinearProgressProp.Value <| map.Progress ]
                    typography [] [
                        str (sprintf "%s%s" (map.Progress/100 |> string) "%")
                    ]
                ]
                tableCell [
                    TableCellProp.Align TableCellAlign.Center
                ] [ 
                    map.Map.FileSize 
                    |> Option.map (fun v -> 
                        sprintf "%s %s" (v |> string) "MB") 
                    |> defStr 
                ]
            ])

    let private tabContent (classes: IClasses) model dispatch =
        grid [
            GridProp.Container true
            GridProp.AlignItems GridAlignItems.Center
            GridProp.Justify GridJustify.Center
            GridProp.Direction GridDirection.Row
            GridProp.Wrap GridWrap.Wrap
            Style [ 
                CSSProp.Display DisplayOptions.Flex
                CSSProp.Width "100%"
            ] 
        ] <|
            match model.TabSelected with
            | Available -> renderAvailableMaps classes model dispatch
            | Installed -> 
                [
                    grid [
                        GridProp.Item true
                        Style [ CSSProp.Width "100%" ]
                    ] [
                        card [
                            MaterialProp.Elevation 2
                            CardProp.Raised true 
                        ] [
                            table [
                            ] [
                                tableHead [] [
                                    tableRow [] [
                                        tableCell [
                                            TableCellProp.Align TableCellAlign.Left
                                        ] [ str "Name" ]
                                        tableCell [
                                            TableCellProp.Align TableCellAlign.Right
                                        ] [ str "Author" ]
                                        tableCell [
                                            TableCellProp.Align TableCellAlign.Right
                                        ] [ str "Version"]
                                        tableCell [
                                            TableCellProp.Align TableCellAlign.Right
                                        ] [ str "Version" ]
                                        tableCell [
                                            TableCellProp.Align TableCellAlign.Right
                                        ] [ str "File Size" ]
                                        tableCell [
                                            TableCellProp.Align TableCellAlign.Right
                                        ] [ str "Players" ]
                                    ]
                                ]
                                tableBody [] <| renderInstalledMaps classes model dispatch
                            ]
                        ]
                    ]
                ]
            | Installing ->
                [
                    grid [
                        GridProp.Item true
                        Style [ CSSProp.Width "100%" ]
                    ] [
                        card [
                            MaterialProp.Elevation 2
                            CardProp.Raised true 
                        ] [
                            table [
                                Style [ CSSProp.Width "100%" ]
                            ] [
                                tableHead [] [
                                    tableRow [] [
                                        tableCell [
                                            TableCellProp.Align TableCellAlign.Left
                                        ] [ str "Name" ]
                                        tableCell [
                                            TableCellProp.Align TableCellAlign.Center
                                            Style [ CSSProp.Width "15em" ]
                                        ] [ str "Progress" ]
                                        tableCell [
                                            TableCellProp.Align TableCellAlign.Center
                                        ] [ str "Downloaded" ]
                                    ]
                                ]
                                tableBody [] <| renderInstallingMaps classes model dispatch
                            ]
                        ]
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
                card [ CardProp.Raised true ] [
                    tabs [
                        HTMLAttr.Value (model.TabSelected.GetTag)
                        TabsProp.Variant TabsVariant.FullWidth
                        TabsProp.ScrollButtons ScrollButtonsType.On
                        TabsProp.IndicatorColor TabsIndicatorColor.Secondary
                        TabsProp.TextColor TabsTextColor.Secondary
                        TabsProp.Centered true
                        TabsProp.OnChange (fun _ tabPicked -> dispatch <| TabSelected(Tab.GetTabFromTag(tabPicked)) )
                    ] [
                        tab [ 
                            HTMLAttr.Label <| Available.Text
                            HTMLAttr.Disabled <| model.Available.IsEmpty
                        ]
                        tab [
                            HTMLAttr.Label <| Installed.Text
                            HTMLAttr.Disabled <| model.Installed.IsEmpty
                        ]
                        tab [
                            HTMLAttr.Disabled <| model.Installing.IsEmpty
                            MaterialProp.Label <|
                                badge [
                                    BadgeProp.Color BadgeColor.Primary
                                    BadgeProp.Max 100
                                    BadgeProp.Invisible (model.Installing.IsEmpty)
                                    BadgeProp.BadgeContent <| ofInt (model.Installing.Length)
                                ] [ str Installing.Text ]
                        ]
                    ]
                    divider []
                    div [
                        Style [ 
                            CSSProp.Padding "2em"
                            CSSProp.MinHeight "5em"
                            CSSProp.MaxHeight "73vh"
                            CSSProp.OverflowY "Scroll"
                        ]
                    ] [ 
                        tabContent classes model dispatch
                    ]
                ]
            yield 
                div [ 
                    Style [
                        CSSProp.MarginTop "auto"
                        CSSProp.MarginLeft "auto"
                    ] 
                ] [
                    button [
                        HTMLAttr.Disabled <| 
                            match model.TabSelected with
                            | Available -> model.Available.IsEmpty
                            | Installed -> model.Installed.IsEmpty
                            | Installing -> model.Installing.IsEmpty
                        ButtonProp.Variant ButtonVariant.Contained
                        MaterialProp.Color ComponentColor.Primary
                        DOMAttr.OnClick <| fun _ -> 
                            dispatch InstallAll
                        Style [ 
                            CSSProp.MaxHeight "2.6em"
                            CSSProp.MarginTop "auto"
                            CSSProp.MarginLeft "auto"
                        ]
                    ] [ 
                        yield
                            match model.TabSelected with
                            | Available -> str "Install All"
                            | Installed -> str "Uninstall All"
                            | Installing -> str "Cancel All"
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
