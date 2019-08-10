namespace MordhauBuddy.App.ContextMenu

module Types =
    open Fable.Core
    open Fable.Import

    module private Actions =
        [<Emit("document.execCommand(\"Cut\")")>]
        let cut () : unit = jsNative

        [<Emit("document.execCommand(\"Copy\")")>]
        let copy () : unit = jsNative

        [<Emit("document.execCommand(\"Paste\")")>]
        let paste () : unit = jsNative

        [<Emit("document.execCommand(\"SelectAll\")")>]
        let selectAll () : unit = jsNative

    type ContextAction =
        | Cut
        | Copy
        | Paste
        | SelectAll
        member this.GetAction =
            match this with
            | Cut -> fun () -> Actions.cut()
            | Copy -> fun () -> Actions.copy()
            | Paste -> fun () -> Actions.paste()
            | SelectAll -> fun () -> Actions.selectAll()

    type MenuItem =
        { Label : string
          Action : ContextAction }

    type MenuPosition =
        { X : int
          Y : int }

    type Model =
        { Opened : bool
          Position : MenuPosition
          MenuItems : MenuItem list }

    type Msg =
        | Open of Browser.Types.MouseEvent
        | Close
        | Action of ContextAction
