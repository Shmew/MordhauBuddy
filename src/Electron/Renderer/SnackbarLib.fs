namespace MordhauBuddy.App

/// Fairly reusable snackbar Elmish component/library. See Snackbars.fs for example usage.
/// Simple, conforms to Material UI, e.g. no stacking of multiple snackbars.
/// Also no color override, but that should be easy to implement if desired.
module Snack =
    open Elmish
    open Fable.Core
    open Fable.Core.JsInterop
    open Fable.React
    open Fable.React.Props
    open Fable.MaterialUI
    open Fable.MaterialUI.Core
    open Fable.MaterialUI.Props
    open Fable.MaterialUI.MaterialDesignIcons
    open Fable.MaterialUI.Icons

    type Snack<'msg> =
        { Message : string
          ActionTxt : string option
          ActionMsg : 'msg option
          TimeoutMs : int }

    /// Creates a snack with the specified message.
    let create message =
        { Message = message
          ActionTxt = None
          ActionMsg = None
          TimeoutMs = 4000 }

    /// Adds an action to the snack that hides the snack.
    let withDismissAction dismissTxt snack = { snack with ActionTxt = Some dismissTxt }

    /// Adds an action to the snack that hides the snack and causes the specified
    /// message to be dispatched in the command returned by the update function.
    let withCustomAction actionTxt actionMsg snack =
        { snack with ActionTxt = Some actionTxt
                     ActionMsg = Some actionMsg }

    /// Sets the specified timeout for the snack.
    let withTimeout timeoutMs snack = { snack with TimeoutMs = timeoutMs }

    type Msg<'msg> =
        | Add of Snack<'msg>
        | Click of 'msg option
        | NextState of int

    type State<'msg> =
        /// The snackbar is not showing any messages and there are no messages in the queue.
        | Inert
        /// The snackbar is showing the specified message.
        | Active of Snack<'msg>
        /// The snackbar is fading out or waiting before showing the next message.
        | Waiting of Snack<'msg>

    type Model<'msg> =
        { Queue : Snack<'msg> list
          IntervalMs : int
          State : State<'msg>
          CurrentSnackId : int }

    let init() =
        { Queue = []
          IntervalMs = 400
          State = Inert
          CurrentSnackId = 0 }

    /// Adds the specified snack to the snackbar model. Returns the updated model and
    /// commands that must be executed.
    let add snack = Cmd.ofMsg (Add snack)

    let private delayedMsg delayMs msg =
        let asyncMsg =
            async {
                do! Async.Sleep delayMs
                return msg
            }
        Cmd.OfAsync.result asyncMsg

    let update msg m =
        match m.State, msg with
        | _, Add snack ->
            let cmd =
                if m.State = Inert then Cmd.ofMsg (NextState m.CurrentSnackId)
                else Cmd.none
            { m with Queue = m.Queue @ [ snack ] }, cmd, Cmd.none
        | _, NextState i when i <> m.CurrentSnackId -> m, Cmd.none, Cmd.none
        | Inert, NextState _ ->
            match m.Queue with
            | [] -> m, Cmd.none, Cmd.none
            | head :: tail ->
                { m with State = Active head
                         Queue = tail
                         CurrentSnackId = m.CurrentSnackId + 1 },
                delayedMsg head.TimeoutMs (NextState <| m.CurrentSnackId + 1), Cmd.none
        | Active x, NextState _ ->
            { m with State = Waiting x }, delayedMsg m.IntervalMs (NextState m.CurrentSnackId), Cmd.none
        | Waiting _, NextState _ ->
            match m.Queue with
            | [] -> { m with State = Inert }, Cmd.none, Cmd.none
            | head :: tail ->
                { m with State = Active head
                         Queue = tail
                         CurrentSnackId = m.CurrentSnackId + 1 },
                delayedMsg head.TimeoutMs (NextState <| m.CurrentSnackId + 1), Cmd.none
        | _, Click actionMsg ->
            m, Cmd.ofMsg (NextState m.CurrentSnackId),
            actionMsg
            |> Option.map Cmd.ofMsg
            |> Option.defaultValue Cmd.none

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
                        MaterialProp.Color ComponentColor.Secondary
                    ] [ str action ]
                )
            | _ -> ()
        ] []

    // Workaround for using JSS with Elmish
    // https://github.com/mvsmal/fable-material-ui/issues/4#issuecomment-422781471
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
