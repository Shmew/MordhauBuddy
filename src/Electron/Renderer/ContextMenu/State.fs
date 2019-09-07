namespace MordhauBuddy.App.ContextMenu

module State =
    open Elmish
    open Elmish.Bridge
    open MordhauBuddy.App
    open RenderUtils
    open Types

    let init() =
        { Opened = false
          Position =
              { X = 0
                Y = 0 }
          MenuItems = [] }

    let update (msg: Msg) (model: Model) =
        match msg with
        | Open e ->
            if model.Opened then
                model, Cmd.ofMsg Close
            else
                let pos = getMousePositions()
                let element = getElementAtPos pos.X pos.Y

                let classes =
                    element
                    |> Option.bind (fun e -> e.className |> Some)
                    |> defaultArg
                    <| ""

                let actionList =
                    eventPersist e
                    match classes with
                    | l when l.Contains "MuiInput" && not (l.Contains "inputSelect") ->
                        [ { Label = "Cut"
                            Action = Cut }
                          { Label = "Copy"
                            Action = Copy }
                          { Label = "Paste"
                            Action = Paste }
                          { Label = "Select All"
                            Action = SelectAll } ]
                    | _ -> []

                if actionList.IsEmpty then
                    model, Cmd.none
                else
                    { model with
                          Opened = true
                          Position =
                              { model.Position with
                                    X = pos.X
                                    Y = pos.Y }
                          MenuItems = actionList }, Cmd.none
        | Close -> { model with Opened = false }, Cmd.none
        | Action(f) ->
            f.GetAction()
            model, Cmd.ofMsg Close
