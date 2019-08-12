namespace MordhauBuddy.App.Snackbar

module View =
    open Fable.Core.JsInterop
    open Fable.React
    open Fable.React.Props
    open Fable.MaterialUI
    open Fable.MaterialUI.Core
    open Types

    let private styles (theme : ITheme) : IStyles list = []

    let private view' (classes: IClasses) model dispatch =
        snackbar [
            yield AnchorOrigin ({vertical = SnackbarVerticalOrigin.Bottom; horizontal = SnackbarHorizontalOrigin.Left})
            yield Open (match model.State with Active _ -> true | _ -> false)
            match model.State with Active x | Waiting x -> yield Message (str x.Message) | _ -> ()
            match model.State with
            | Active { ActionTxt = Some action; ActionMsg = p }
            | Waiting { ActionTxt = Some action; ActionMsg = p } ->
                yield SnackbarProp.Action (
                    button [
                        Key action
                        OnClick (fun _ -> p |> Click |> dispatch)
                        MaterialProp.Color ComponentColor.Primary
                        ButtonProp.Variant ButtonVariant.Contained
                    ] [ str action ]
                )
            | _ -> ()
        ] []

    /// Workaround for using JSS with Elmish
    /// https://github.com/mvsmal/fable-material-ui/issues/4#issuecomment-422781471
    type private IProps<'msg> =
        abstract model : Model<'msg> with get, set
        abstract dispatch : (Msg<'msg> -> unit) with get, set
        inherit IClassesProps

    type private Component<'msg>(p) =
        inherit PureStatelessComponent<IProps<'msg>>(p)
        let viewFun (p : IProps<'msg>) = view' p.classes p.model p.dispatch
        let viewWithStyles = withStyles (StyleType.Func styles) [] viewFun
        override this.render() = ReactElementType.create viewWithStyles this.props []

    let view (model : Model<'msg>) (dispatch : Msg<'msg> -> unit) : ReactElement =
        let props =
            jsOptions<IProps<'msg>> (fun p ->
                p.model <- model
                p.dispatch <- dispatch)
        ofType<Component<'msg>, _, _> props []
