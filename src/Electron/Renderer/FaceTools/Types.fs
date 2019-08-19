namespace MordhauBuddy.App.FaceTools

module rec Types =
    open System
    open Fable.Core
    open Fable.React
    open Fable.React.Props
    open Fable.MaterialUI
    open Fable.MaterialUI.Core
    open FSharp.Core  /// To avoid shadowing Result<_,_>
    open MordhauBuddy.App
    open RenderUtils
    open RenderUtils.Directory
    open MordhauBuddy.Shared.ElectronBridge
    open Microsoft.FSharp.Reflection

    type ToggleDirection =
        | Left
        | Right

    type Msg =
        | ClientMsg of BridgeResult
        | StepperSubmit
        | StepperRestart
        | StepperNext
        | StepperBack
        | GetProfiles
        | ToggleAll of ToggleDirection * bool
        | Toggle of ToggleDirection * Profile
        | Move of ToggleDirection
        | TabSelected of int
        | SetImportString of string
        | ValidateImport
        | CopiedClipboard
        | SnackMsg of Snackbar.Types.Msg<Msg>
        | SnackDismissMsg

    type Steps =
        | ChooseProfiles
        | ChooseAction
        member this.Text =
            this.ToString()
            |> String.duToTitle

        member this.StepCaption =
            match this with
            | ChooseProfiles -> [ str "Choose which profiles you'd like to modify." ]
            | ChooseAction ->
                [ div [] [str "Choose the type of action you'd like to make."]
                  div [ Style [ CSSProp.PaddingTop "1em" ] ] [
                    str "A backup will be created of your Game.ini file in a \
                        child folder of that directory if modifying settings." 
                  ]
                ]

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

        member this.GetTag =
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
            this.GetTag + 1
            |> Array.tryItem <| Steps.Cases
            |> function
            | Some(i) -> i.Name
            | None -> 
                Steps.Cases
                |> Array.head
                |> (fun t -> t.Name)
            |> Steps.Instantiate

        member this.Back =
            this.GetTag - 1
            |> Array.tryItem <| Steps.Cases
            |> function
            | Some(i) -> i.Name
            | None -> 
                Steps.Cases
                |> Array.head
                |> (fun t -> t.Name)
            |> Steps.Instantiate

        member this.Buttons dispatch model =
                match model.StepperComplete with
                | false ->
                    let isDisabled =
                        match this with
                        | ChooseProfiles when model.TransferList.RightProfiles.Length = 0 -> true
                        | ChooseAction when (not model.Import.Validated && model.TabSelected = 2) || model.TabSelected = 3 -> true
                        | _ when model.Submit.Waiting -> true
                        | _ -> false

                    [
                        button [
                            HTMLAttr.Disabled (this.GetTag = 0)
                            DOMAttr.OnClick <| fun _ -> dispatch (StepperBack)
                        ] [ str "Back" ]
                        button [
                            HTMLAttr.Disabled isDisabled
                            ButtonProp.Variant ButtonVariant.Contained
                            MaterialProp.Color ComponentColor.Primary
                            DOMAttr.OnClick <| fun _ -> 
                                dispatch (if this.Last then StepperSubmit else StepperNext)
                            Style [ CSSProp.MaxHeight "2.6em" ]
                        ] [ 
                            if model.Submit.Waiting then 
                                yield 
                                    circularProgress [ 
                                        CircularProgressProp.Size (CircularProgressSize.Case1(20))
                                        Style [ CSSProp.MaxHeight "2.6em" ] 
                                    ]
                            else yield str <| if this.Last then "Submit" else "Next"
                        ]
                    ]
                | true ->
                    [
                        button [
                            DOMAttr.OnClick <| fun _ -> dispatch (StepperRestart)
                        ] [ str "Restart" ]
                    ]
                |> div [ 
                    Style [
                        CSSProp.MarginTop "auto"
                        CSSProp.MarginLeft "auto"
                    ] 
                ]

        member this.StepElems complete =
            let isComplete i =
                match complete,this.GetTag > i with
                | true, _ -> true
                | _, true -> true
                | _ -> false

            Steps.GetSteps
            |> Array.map (fun stepCase ->
                step [ StepProp.Completed <| isComplete stepCase.GetTag ] [
                    stepLabel [] [str stepCase.Text]
                ])

    type Profile = 
        { Name : string
          Checked : bool
          Export : string }

    type TransferList =
        { LeftProfiles : Profile list
          LeftChecked : int
          RightProfiles : Profile list
          RightChecked : int 
          Error : bool
          HelperText : string }

    type ImportStr =
        { ImportString : string
          Error : bool
          HelperText : string
          Validated : bool }

    type Submit =
        { Waiting : bool
          Error : bool
          HelperText : string
          Complete : bool }

    type Model = 
        { Stepper : Steps
          StepperComplete : bool
          GameDir : ConfigDir 
          TransferList : TransferList
          TabSelected : int
          Import : ImportStr
          Submit : Submit
          Snack : Snackbar.Types.Model<Msg> }
