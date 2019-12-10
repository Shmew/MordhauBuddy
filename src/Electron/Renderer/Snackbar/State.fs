namespace MordhauBuddy.App.Snackbar

module State =
    open Elmish
    open Types

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
        { snack with
              ActionTxt = Some actionTxt
              ActionMsg = Some actionMsg }

    /// Sets the specified timeout for the snack.
    let withTimeout timeoutMs snack = { snack with TimeoutMs = timeoutMs }

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
        Cmd.OfAsyncImmediate.result asyncMsg

    let update msg m =
        match m.State, msg with
        | _, Add snack ->
            let cmd =
                if m.State = Inert then Cmd.ofMsg (NextState m.CurrentSnackId) else Cmd.none
            { m with Queue = m.Queue @ [ snack ] }, cmd, Cmd.none
        | _, NextState i when i <> m.CurrentSnackId -> m, Cmd.none, Cmd.none
        | Inert, NextState _ ->
            match m.Queue with
            | [] -> m, Cmd.none, Cmd.none
            | head :: tail ->
                { m with
                      State = Active head
                      Queue = tail
                      CurrentSnackId = m.CurrentSnackId + 1 },
                delayedMsg head.TimeoutMs (NextState <| m.CurrentSnackId + 1), Cmd.none
        | Active x, NextState _ ->
            { m with State = Waiting x }, delayedMsg m.IntervalMs (NextState m.CurrentSnackId), Cmd.none
        | Waiting _, NextState _ ->
            match m.Queue with
            | [] -> { m with State = Inert }, Cmd.none, Cmd.none
            | head :: tail ->
                { m with
                      State = Active head
                      Queue = tail
                      CurrentSnackId = m.CurrentSnackId + 1 },
                delayedMsg head.TimeoutMs (NextState <| m.CurrentSnackId + 1), Cmd.none
        | _, Click actionMsg ->
            m, Cmd.ofMsg (NextState m.CurrentSnackId),
            actionMsg
            |> Option.map Cmd.ofMsg
            |> Option.defaultValue Cmd.none
