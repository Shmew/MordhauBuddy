namespace MordhauBuddy.App.ContextMenu

module View =
    open Fable.Core.JsInterop
    open Fable.React
    open Fable.React.Props
    open Fable.MaterialUI
    open Fable.MaterialUI.Core
    open Fable.MaterialUI.MaterialDesignIcons
    open Fable.MaterialUI.Icons
    open FSharp.Core /// To avoid shadowing Result<_,_>
    open MordhauBuddy.App
    open RenderUtils
    open RenderUtils.String
    open RenderUtils.Validation
    open RenderUtils.MaterialUI
    open RenderUtils.MaterialUI.Core
    open Elmish.React
    open Electron
    open Types

    let private styles (theme: ITheme): IStyles list = []

    let private menuItems (classes: IClasses) model dispatch =
        div [] <| (model.MenuItems
                   |> List.map (fun item ->
                       menuItem
                           [ DOMAttr.OnClick <| fun _ -> dispatch <| Action(item.Action)
                             Style [ CSSProp.MinHeight "0em" ] ] [ str item.Label ]))

    let private view' (classes: IClasses) model dispatch =
        menu
            [ MenuProp.DisableAutoFocusItem true
              MaterialProp.KeepMounted true
              MaterialProp.Open model.Opened
              MaterialProp.OnClose <| fun _ -> dispatch Close
              PopoverProp.AnchorReference AnchorReference.AnchorPosition
              PopoverProp.AnchorPosition
                  { left = (model.Position.X)
                    top = (model.Position.Y) } ]
            [ if model.Opened then
                yield clickAwayListener [ ClickAwayListenerProp.OnClickAway <| fun _ -> dispatch Close ]
                          [ menuItems classes model dispatch ] ]

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
