namespace MordhauBuddy.App.Community

module View =
    open Fable.Core.JsInterop
    open Fable.React
    open Fable.React.Props
    open Fable.MaterialUI
    open Fable.MaterialUI.Core
    open Fable.MaterialUI.MaterialDesignIcons
    open Fable.MaterialUI.Icons
    open FSharp.Core /// To avoid shadowing Result<_,_>
    open Types

    let private styles (theme: ITheme): IStyles list = []

    let private loading (classes: IClasses) model dispatch =
        div [ Style [ CSSProp.Padding "10em" ] ]
            [ typography
                [ TypographyProp.Variant TypographyVariant.H6
                  TypographyProp.Align TypographyAlign.Center
                  Style [ CSSProp.PaddingBottom "5em" ] ] [ str "Preparing for battle..." ]
              circularProgress
                  [ CircularProgressProp.Size <| CircularProgressSize.Case2 "5em"
                    Style [ CSSProp.MarginLeft "45%" ] ] ]

    let getSteamAnnouncements (classes: IClasses) model dispatch =
        model.SteamAnnouncements
        |> List.map (fun (title, body) ->
            [ grid
                [ GridProp.Item true
                  Style [ CSSProp.Width "100%" ] ]
                  [ card
                      [ CardProp.Raised true
                        MaterialProp.Elevation 2
                        Style
                            [ CSSProp.MarginBottom "2em"
                              CSSProp.Padding "1em" ] ]
                        [ typography
                            [ TypographyProp.Align TypographyAlign.Center
                              TypographyProp.Variant TypographyVariant.H6
                              Style [ CSSProp.PaddingBottom "2em" ] ] [ str title ]
                          div
                              [ DOMAttr.DangerouslySetInnerHTML { __html = body }
                                Style [ CSSProp.PaddingBottom "2em" ] ] [] ] ] ])
        |> List.concat

    let private tabContent (classes: IClasses) model dispatch =
        grid
            [ GridProp.Container true
              GridProp.AlignItems GridAlignItems.Center
              GridProp.Justify GridJustify.Center
              GridProp.Direction GridDirection.Row
              GridProp.Wrap GridWrap.Wrap
              Style
                  [ CSSProp.Display DisplayOptions.Flex
                    CSSProp.FlexGrow 1 ] ]
        <| match model.TabSelected with
           | Announcements ->
               match getSteamAnnouncements classes model dispatch with
               | [] ->
                   [ typography
                       [ TypographyProp.Align TypographyAlign.Center
                         TypographyProp.Variant TypographyVariant.H6 ] [ str "No announcements found." ] ]
               | rList -> rList

    let private tabs (classes: IClasses) model dispatch =
        [ tabs
            [ HTMLAttr.Value(model.TabSelected.GetTag)
              TabsProp.Variant TabsVariant.FullWidth
              TabsProp.ScrollButtons ScrollButtonsType.On
              TabsProp.IndicatorColor TabsIndicatorColor.Secondary
              TabsProp.TextColor TabsTextColor.Secondary
              TabsProp.Centered true
              TabsProp.OnChange(fun _ tabPicked -> dispatch <| TabSelected(Tab.GetTabFromTag(tabPicked))) ]
              [ tab [ HTMLAttr.Label <| Announcements.Text ] ]
          divider []
          div
              [ Style
                  [ CSSProp.Padding "2em"
                    CSSProp.MinHeight "5em"
                    CSSProp.MaxHeight "77vh"
                    CSSProp.MarginBottom "1vh"
                    CSSProp.OverflowY OverflowOptions.Auto ] ] [ tabContent classes model dispatch ] ]

    let private view' (classes: IClasses) model dispatch =
        card
            [ CardProp.Raised true
              Style
                  [ CSSProp.FlexDirection "column"
                    CSSProp.Display DisplayOptions.Flex
                    CSSProp.Height "inherit"
                    CSSProp.OverflowX OverflowOptions.Hidden ] ]
        <| if model.LoadingElem then [ loading classes model dispatch ]
           else tabs classes model dispatch

    /// Workaround for using JSS with Elmish
    /// https://github.com/mvsmal/fable-material-ui/issues/4#issuecomment-422781471
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
