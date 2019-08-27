namespace MordhauBuddy.App.Snackbar

module Types =
    type Snack<'msg> =
        { Message: string
          ActionTxt: string option
          ActionMsg: 'msg option
          TimeoutMs: int }

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
        { Queue: Snack<'msg> list
          IntervalMs: int
          State: State<'msg>
          CurrentSnackId: int }
