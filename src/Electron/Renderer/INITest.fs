namespace MordhauBuddy.App

module INITest =
    open System
    open Fable.Core
    open Fable.Core.JsInterop
    open Fable.React
    open Fable.React.Props
    open Fable.MaterialUI
    open Fable.MaterialUI.Core
    open Fable.MaterialUI.Props
    open Fable.MaterialUI.MaterialDesignIcons
    open Fable.MaterialUI.Icons
    open FSharp.Core
    open Utils
    open Elmish
    open Elmish.Bridge
    open MordhauBuddy.Shared.ElectronBridge
    open BridgeUtils
    open Microsoft.FSharp.Reflection

    type Msg =
        | ServerMsg of RemoteServerMsg
        | ClientMsg of BridgeResult
        | StepperSubmit
        | StepperRestart
        | StepperNext
        | StepperBack

    type Steps =
        | LocateConfig
        | ChooseFace
        | ApplyChanges
        member this.Text =
            this.ToString()
            |> String.duToTitle

        member this.StepCaption =
            match this with
            | LocateConfig -> "Find where your configuration files are located."
            | ChooseFace -> "Choose which type of action you'd like to take."
            | ApplyChanges -> "Submit the changes."

        static member private Cases =
            FSharpType.GetUnionCases typeof<Steps>

        static member private Instantiate name =
            Steps.Cases
            |> Array.tryFind (fun uc -> uc.Name = name)
            |> Option.map (fun uc -> 
                Reflection.FSharpValue.MakeUnion( uc, [||] ) :?> Steps)
            |> Option.get

        static member GetSteps =
            Steps.Cases
            |> Array.map (fun uc ->
                uc.Name |> Steps.Instantiate)

        member this.StepValue =
            Steps.Cases
            |> Seq.tryFind (fun uc -> uc.Name = this.ToString())
            |> Option.map (fun uc -> uc.Tag)
            |> Option.get

        static member First =
            Steps.Cases
            |> Array.head
            |> (fun t -> t.Name) 
            |> Steps.Instantiate 

        member this.Last =
            Steps.Cases 
            |> Array.last 
            |> (fun t -> t.Name) 
            |> Steps.Instantiate 
            |> (=) this
            
        member this.Next =
            this.StepValue + 1
            |> Array.tryItem <| Steps.Cases
            |> function
            | Some(i) -> i.Name
            | None -> 
                Steps.Cases
                |> Array.head
                |> (fun t -> t.Name)
            |> Steps.Instantiate

        member this.Back =
            this.StepValue - 1
            |> Array.tryItem <| Steps.Cases
            |> function
            | Some(i) -> i.Name
            | None -> 
                Steps.Cases
                |> Array.head
                |> (fun t -> t.Name)
            |> Steps.Instantiate

        member this.Buttons dispatch complete =
            if not complete then
                div [] [
                    button [
                        HTMLAttr.Disabled (this.StepValue = 0)
                        DOMAttr.OnClick <| fun _ -> dispatch (StepperBack)
                    ] [ str "Back" ]
                    button [
                        ButtonProp.Variant ButtonVariant.Contained
                        MaterialProp.Color ComponentColor.Primary
                        DOMAttr.OnClick <| fun _ -> 
                            dispatch (if this.Last then StepperSubmit else StepperNext)
                    ] [ str <| if this.Last then "Submit" else "Next" ]
                ]
            else
                div [] [
                    button [
                        DOMAttr.OnClick <| fun _ -> dispatch (StepperRestart)
                    ] [ str "Restart" ]
                ]

        member this.StepElems complete =
            let isComplete i =
                match complete,this.StepValue > i with
                | true, _ -> true
                | _, true -> true
                | _ -> false

            Steps.GetSteps
            |> Array.map (fun stepCase ->
                step [StepProp.Completed <| isComplete stepCase.StepValue] [
                    stepLabel [] [str stepCase.Text]
                ])

        member this.Content =
            ()

    type Model = 
        { Message : string
          Stepper : Steps
          StepperComplete : bool }

    let init() =
        { Message = "init"
          Stepper = LocateConfig
          StepperComplete = false }

    let update (msg: Msg) (model: Model) =
        match msg with
        | ServerMsg sMsg ->
            model
        | ClientMsg cMsg ->
            match cMsg with
            | BridgeResult.Text(s) ->
                { model with Message = s}
            | BridgeResult.TextList(sList) ->
                { model with Message = sList |> List.reduce (fun acc elem -> acc + " " + elem)}
            | _ -> model
        | StepperSubmit -> { model with StepperComplete = true }
        | StepperRestart -> init()
        | StepperNext -> { model with Stepper = model.Stepper.Next }
        | StepperBack -> { model with Stepper = model.Stepper.Back }

    // Domain/Elmish above, view below
    let private styles (theme : ITheme) : IStyles list = []

    let private view' (classes: IClasses) model dispatch =
        div [] [
            stepper [ActiveStep (model.Stepper.StepValue)]
                <| model.Stepper.StepElems model.StepperComplete
            str model.Stepper.StepCaption
            model.Stepper.Buttons dispatch model.StepperComplete
            str (model.Message)
            iconButton [
                DOMAttr.OnClick <| fun _ -> dispatch (INI.Ops.getGameProfiles |> ServerMsg)
            ] [ str "Send msg to server" ]
        ]

    // Workaround for using JSS with Elmish
    // https://github.com/mvsmal/fable-material-ui/issues/4#issuecomment-422781471
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
