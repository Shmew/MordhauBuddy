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
    open MordhauBuddy.Shared.ElectronBridge

    let private styles (theme : ITheme) : IStyles list = [
        Styles.Custom ("darkList", [
            CSSProp.BackgroundColor theme.palette.background.``default``
        ])
    ]

    let private addMapErrorTooltip (elem : ReactElement) (error : string) =
        tooltip [
            TooltipProp.Title (error |> str)
        ] [ elem ]

    let private tableMenu (classes: IClasses) model dispatch (map : CommunityMapWithState) =
        div [] [
            yield
                iconButton [
                    DOMAttr.OnClick <| fun _ -> dispatch (ToggleMenu(model.TabSelected, map.Map.Folder)) 
                ] [ moreVertIcon [] ]
            match map.MenuState with
            | MenuState.Open(pos) ->
                yield
                    menu [
                        MaterialProp.Open true
                        MaterialProp.KeepMounted true
                        PopoverProp.AnchorReference AnchorReference.AnchorPosition
                        PopoverProp.AnchorPosition { left = pos.X; top = pos.Y }
                    ] [
                        clickAwayListener [
                            ClickAwayListenerProp.OnClickAway <| fun _ -> dispatch (ToggleMenu(model.TabSelected, map.Map.Folder)) 
                        ] [
                            div [] <|
                                match model.TabSelected with
                                | Installed -> 
                                    [ menuItem [
                                        HTMLAttr.Disabled (model.Available |> List.exists (fun m -> m.Map.Folder = map.Map.Folder) |> not)
                                        DOMAttr.OnClick <| fun _ -> dispatch (Install(map.Map.GetName(), map.Map.Folder))
                                      ] [ str "Update" ]
                                      menuItem [
                                          DOMAttr.OnClick <| fun _ -> dispatch (Uninstall(map.Map.Folder))
                                      ] [ str "Uninstall" ] ]
                                | Installing -> 
                                    [ menuItem [
                                          DOMAttr.OnClick <| fun _ -> dispatch (CancelInstall(map.Map.Folder))
                                      ] [ str "Cancel" ] ]
                                | _ -> []
                        ]
                    ]
            | _ -> ()
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
                        CardHeaderProp.Title <| str (map.Map |> getName)
                        CardHeaderProp.Subheader <| str (map.Map.Version.GetString())
                        CardHeaderProp.Action <|
                            fab [
                                MaterialProp.Color ComponentColor.Secondary
                                FabProp.Size FabSize.Medium
                                DOMAttr.OnClick <| fun _ -> dispatch (Install(map.Map.GetName(), map.Map.Folder))
                            ] [ addIcon [] ]
                    ] []
                    cardMedia [
                        match map.Map.Image with
                        | Some(mapImage) ->
                            yield CardMediaProp.Image mapImage
                            yield HTMLAttr.Title <| getName map.Map
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
                        (map.Map.GetMetaData() 
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
            tableRow [
            ] [
                tableCell [
                    TableCellProp.Align TableCellAlign.Left
                ] [
                    typography [
                        if map.State.IsStateError then
                            yield TypographyProp.Color TypographyColor.Error
                        yield TypographyProp.Variant TypographyVariant.Body2
                    ] [ str (map.Map |> getName) ]
                ]
                tableCell [
                    TableCellProp.Align TableCellAlign.Left
                    Style [ CSSProp.MinWidth "10em" ]
                ] [
                    card [
                        
                    ] [
                        cardMedia [
                            match map.Map.Image with
                            | Some(mapImage) ->
                                yield CardMediaProp.Image mapImage
                                yield HTMLAttr.Title <| getName map.Map
                            | _ ->
                                yield HTMLAttr.Title "No picture provided"
                            yield 
                                Style [
                                    CSSProp.Height "5em"
                                ]
                        ]
                    ]
                ]
                tableCell [
                    TableCellProp.Align TableCellAlign.Right
                ] [
                    typography [
                        if map.State.IsStateError then
                            yield TypographyProp.Color TypographyColor.Error
                        yield TypographyProp.Variant TypographyVariant.Body2
                    ] [ map.Map.Author |> defStr ]
                ]
                tableCell [
                    TableCellProp.Align TableCellAlign.Right
                    Style [ CSSProp.MinWidth "8em" ]
                ] [
                    typography [
                        if map.State.IsStateError then
                            yield TypographyProp.Color TypographyColor.Error
                        yield TypographyProp.Variant TypographyVariant.Body2
                    ] [ map.Map.GetDate() |> defStr ]
                ]
                tableCell [
                    TableCellProp.Align TableCellAlign.Right
                ] [
                    typography [
                        if map.State.IsStateError then
                            yield TypographyProp.Color TypographyColor.Error
                        yield TypographyProp.Variant TypographyVariant.Body2
                    ] [ str <| map.Map.Version.GetString() ]
                ]
                tableCell [
                    TableCellProp.Align TableCellAlign.Right
                    Style [ CSSProp.MinWidth "6em" ]
                ] [
                    typography [
                        if map.State.IsStateError then
                            yield TypographyProp.Color TypographyColor.Error
                        yield TypographyProp.Variant TypographyVariant.Body2
                    ] [ 
                        map.Map.FileSize 
                        |> Option.map (fun v -> 
                            sprintf "%s %s" (v |> string) "MB") 
                        |> defStr 
                    ]
                ]
                tableCell [
                    TableCellProp.Align TableCellAlign.Right
                    Style [ CSSProp.MinWidth "6em" ]
                ] [
                    typography [
                        if map.State.IsStateError then
                            yield TypographyProp.Color TypographyColor.Error
                        yield TypographyProp.Variant TypographyVariant.Body2
                    ] [ map.Map.GetPlayers() |> defStr ]
                ]
                tableCell [
                    TableCellProp.Align TableCellAlign.Right
                    Style [ CSSProp.Width "1em" ]
                ] [ tableMenu classes model dispatch map ]
            ]
            |> fun tRow ->
                match map.State with
                | ComMapState.Error(errMsg) ->
                    addMapErrorTooltip tRow errMsg
                | _ -> tRow)

    let private renderInstallingMaps (classes: IClasses) model dispatch =
        let getName (map : MapTypes.CommunityMap) =
            match map.Name with
            | Some(name) -> name
            | None -> map.Folder

        if model.Installing.IsEmpty then
            dispatch <| TabSelected(Tab.Installed)

        model.Installing
        |> List.map (fun map ->
            tableRow [] [
                tableCell [
                    TableCellProp.Align TableCellAlign.Left
                ] [
                    typography [
                        if map.State.IsStateError then
                            yield TypographyProp.Color TypographyColor.Error
                        yield TypographyProp.Variant TypographyVariant.Body2
                    ] [ str (map.Map |> getName) ]
                ]
                tableCell [
                    TableCellProp.Align TableCellAlign.Center
                    Style [ CSSProp.MinWidth "15em" ]
                ] [
                    typography [
                        if map.State.IsStateError then
                            yield TypographyProp.Color TypographyColor.Error
                        yield TypographyProp.Variant TypographyVariant.Body2
                    ] [ 
                        match map.State with
                        | ComMapState.Success i ->
                            yield 
                                linearProgress [ 
                                    LinearProgressProp.Value i 
                                    LinearProgressProp.Variant LinearProgressVariant.Determinate
                                ]
                            yield
                                typography [] [
                                    str (sprintf "%s%s" (i |> string) "%")
                                ]
                        | _ -> ()
                    ]
                ]
                tableCell [
                    TableCellProp.Align TableCellAlign.Center
                    Style [ CSSProp.MinWidth "10em" ]
                ] [
                    typography [
                        if map.State.IsStateError then
                            yield TypographyProp.Color TypographyColor.Error
                        yield TypographyProp.Variant TypographyVariant.Body2
                    ] [ 
                        map.Map.FileSize 
                        |> Option.map (fun v -> 
                            let progress =
                                match map.State with
                                | ComMapState.Success i when i = 100 -> v
                                | ComMapState.Success i -> 
                                    float(i)
                                    |> fun f -> f / 100. 
                                    |> (*) v
                                | _ -> 0. * 1.<MB>
                            sprintf "%.1f / %s %s" progress (v |> string) "MB") 
                        |> defStr 
                    ]
                ]
                tableCell [
                    TableCellProp.Align TableCellAlign.Right
                    Style [ CSSProp.Width "1em" ]
                ] [ tableMenu classes model dispatch map ]
            ]
            |> fun tRow ->
                match map.State with
                | ComMapState.Error(errMsg) ->
                    addMapErrorTooltip tRow errMsg
                | _ -> tRow)

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
            | Available -> 
                let available = renderAvailableMaps classes model dispatch
                if available.IsEmpty then
                    [ 
                        typography [ 
                            TypographyProp.Align TypographyAlign.Center
                            TypographyProp.Variant TypographyVariant.H6
                        ] [ str "No maps available for download." ] ]
                else available
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
                                tableHead [
                                ] [
                                    tableRow [] [
                                        tableCell [
                                            TableCellProp.Align TableCellAlign.Left
                                        ] [ str "Name" ]
                                        tableCell [
                                            TableCellProp.Align TableCellAlign.Left
                                        ] []
                                        tableCell [
                                            TableCellProp.Align TableCellAlign.Right
                                        ] [ str "Author" ]
                                        tableCell [
                                            TableCellProp.Align TableCellAlign.Right
                                        ] [ str "Release Date"]
                                        tableCell [
                                            TableCellProp.Align TableCellAlign.Right
                                        ] [ str "Version" ]
                                        tableCell [
                                            TableCellProp.Align TableCellAlign.Right
                                        ] [ str "File Size" ]
                                        tableCell [
                                            TableCellProp.Align TableCellAlign.Right
                                        ] [ str "Players" ]
                                        tableCell [
                                            TableCellProp.Align TableCellAlign.Right
                                        ] []
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
                                        tableCell [
                                            TableCellProp.Align TableCellAlign.Right
                                        ] []
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
                        tab [ HTMLAttr.Label <| Available.Text ]
                        tab [ HTMLAttr.Label <| Installed.Text ]
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
                            match model.TabSelected with
                            | Available -> dispatch InstallAll
                            | Installed -> dispatch UninstallAll
                            | Installing -> dispatch CancelInstallAll
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
