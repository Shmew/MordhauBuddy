namespace MordhauBuddy.App.EngineTools

module State =
    open FSharp.Core  // To avoid shadowing Result<_,_>
    open MordhauBuddy.App
    open RenderUtils
    open RenderUtils.Validation
    open Elmish
    open Elmish.Bridge
    open MordhauBuddy.Shared.ElectronBridge
    open BridgeUtils
    open RenderUtils.Directory
    open Types

    let trySavedConfigDir () =
        match ElectronStore.store.get("configDir", defaultValue = "") |> string with
        | "" -> None
        | s -> Some(s)

    let init() =
        { Waiting = true
          ParseWaiting = false
          StepperComplete = false
          ConfigDir = 
            { Directory = (string (defaultArg (trySavedConfigDir()) ""))
              Error = false
              HelperText = "" 
              Validated = false }
          Submit =
            { Waiting = false
              Error = false
              HelperText = ""
              Complete = false }
          Snack = Snackbar.State.init() }

    let update (msg: Msg) (model: Model) =
        let submissionFailed (s: string) =
            { model with
                Submit =
                    { model.Submit with
                        Waiting = false
                        Error = true
                        HelperText = s } }
        match msg with
        | ClientMsg bRes ->
            match bRes with
            | BridgeResult.DefaultDir dOpt ->
                match dOpt with 
                | Some(d) ->
                    { model with
                        Waiting = false
                        ConfigDir =
                            { model.ConfigDir with
                                ConfigDir.Directory = d 
                                ConfigDir.HelperText = "Mordhau directory located"
                                ConfigDir.Validated = false } }, Cmd.ofMsg <| SetConfigDir (d,Ok d)
                | None ->
                    { model with
                        Waiting = false
                        ConfigDir =
                            { model.ConfigDir with
                                ConfigDir.HelperText = "Unable to automatically detect Mordhau directory"
                                ConfigDir.Validated = false } }, Cmd.none
            | BridgeResult.Exists b ->
                { model with
                    Waiting = false
                    ConfigDir =
                        if b then
                            { model.ConfigDir with
                                Error = false
                                HelperText = "Game.ini located"
                                Validated = true } 
                        else
                            { model.ConfigDir with
                                Error = true
                                HelperText = "Game.ini not found"
                                Validated = false } 
                    }, Cmd.none
            | BridgeResult.Parse b ->
                if b then
                    model, Cmd.namedBridgeSend "INI" (INI.Faces.getProfileList)
                else
                    { model with
                        Waiting = false
                        ConfigDir =
                            { model.ConfigDir with
                                Error = true
                                HelperText = "Error parsing Game.ini"}}, Cmd.none
            | BridgeResult.Backup b ->
                submissionFailed "Error creating backup", Cmd.ofMsg SnackDismissMsg
            | BridgeResult.CommitChanges b ->
                if b then
                    { model with
                        StepperComplete = true
                        Submit =
                            { model.Submit with
                                Waiting = false
                                Error = false
                                HelperText = "Changes successfully completed!" } }
                else submissionFailed "Error commiting changes to the file"
                ,Cmd.ofMsg SnackDismissMsg
            | _ -> { model with Waiting = false }, Cmd.none
        | GetDefaultDir ->
            model, Cmd.namedBridgeSend "INI" (INI.Ops.defDir)
        | SetConfigDir (s,res) -> 
            match res with
            | Ok s ->
                { model with
                    ConfigDir =
                        { model.ConfigDir with
                            Directory = s
                            Error = false
                            HelperText = "" } },
                Cmd.namedBridgeSend "INI" (INI.Ops.exists { File = "Game.ini"; WorkingDir = Some(s) })
            | Error _ ->
                { model with
                    ConfigDir =
                        { model.ConfigDir with
                            Directory = s
                            Error = true
                            HelperText = errorStrings res } },
                Cmd.none
        | RequestLoad ->
            let handleLoaded =
                function
                | DirSelect.Selected s ->
                    (s, validateConfigDir s)
                    |> SetConfigDir
                | DirSelect.Canceled -> LoadCanceled
            model, Cmd.OfPromise.perform selectDir () handleLoaded
        | LoadCanceled -> model, Cmd.none
        | WaitingStart msg' ->
            { model with Waiting = true }, Cmd.ofMsg msg'
        | SnackMsg msg' ->
            let m, cmd, actionCmd = Snackbar.State.update msg' model.Snack
            { model with Snack = m },
            Cmd.batch [ Cmd.map SnackMsg cmd
                        actionCmd ]
        | SnackDismissMsg ->
            let cmd =
                Snackbar.State.create model.Submit.HelperText
                |> Snackbar.State.withDismissAction "OK"
                |> Snackbar.State.withTimeout 80000
                |> Snackbar.State.add
            model, Cmd.map SnackMsg cmd
