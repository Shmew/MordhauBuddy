namespace MordhauBuddy.App.MordhauConfig

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

    let init(gameDir: string, gameUserDir: string) =
        { Waiting = true
          Complete = false
          Panels = ExpansionPanels.Init()
          GameDir = 
              { Directory = gameDir
                Error = false
                HelperText = "" 
                Validated = false }
          GameUserDir =
              { Directory = gameUserDir
                Error = false
                HelperText = "" 
                Validated = false }
          Submit =
              { Waiting = false
                Error = false
                HelperText = ""
                Complete = false }
          Snack = Snackbar.State.init() }

    let sender = new INISender(Caller.MordhauConfig)

    let update (msg: Msg) (model: Model) =
        let submissionFailed (s: string) =
            { model with
                Submit =
                    { model.Submit with
                        Waiting = false
                        Error = true
                        HelperText = s } }
        match msg with
        | ClientMsg bMsg ->
            match bMsg.BridgeResult with
            | BridgeResult.DefaultDir dOpt ->
                match dOpt with 
                | Some(d) ->
                    { model with
                        Waiting = false
                        GameDir =
                            { model.GameDir with
                                ConfigDir.Directory = d 
                                ConfigDir.HelperText = "Mordhau directory located"
                                ConfigDir.Validated = false }
                        GameUserDir =
                            { model.GameUserDir with
                                ConfigDir.Directory = d 
                                ConfigDir.HelperText = "Mordhau directory located"
                                ConfigDir.Validated = false } }, Cmd.ofMsg <| SetConfigDir (d,Ok d)
                | None ->
                    { model with
                        Waiting = false
                        GameDir =
                            { model.GameDir with
                                ConfigDir.HelperText = "Unable to automatically detect Mordhau directory"
                                ConfigDir.Validated = false }
                        GameUserDir =
                            { model.GameUserDir with
                                ConfigDir.HelperText = "Unable to automatically detect Mordhau directory"
                                ConfigDir.Validated = false } }, Cmd.none
            | BridgeResult.Exists b ->
                { model with
                    Waiting = false
                    GameDir =
                        if b then
                            { model.GameDir with
                                Error = false
                                HelperText = "Game.ini located"
                                Validated = true } 
                        else
                            { model.GameDir with
                                Error = true
                                HelperText = "Game.ini not found"
                                Validated = false } 
                    }, Cmd.none
            | BridgeResult.Parse b ->
                    { model with
                        Waiting = false
                        GameDir =
                            { model.GameDir with
                                Error = true
                                HelperText = "Error parsing Engine.ini"}}, Cmd.none
            | BridgeResult.Backup b -> model,Cmd.none
            | BridgeResult.CommitChanges b ->
                if b then
                    { model with
                        Complete = true
                        Submit =
                            { model.Submit with
                                Waiting = false
                                Error = false
                                HelperText = "Changes successfully completed!" } }
                else submissionFailed "Error commiting changes to the file"
                ,Cmd.ofMsg SnackDismissMsg
            | _ -> { model with Waiting = false }, Cmd.none
        | GetDefaultDir ->
            model, Cmd.namedBridgeSend "INI" (sender.defDir)
        | SetConfigDir (s,res) -> 
            match res with
            | Ok s ->
                { model with
                    GameDir =
                        { model.GameDir with
                            Directory = s
                            Error = false
                            HelperText = "" } },
                Cmd.namedBridgeSend "INI" (sender.exists { File = File.Engine; WorkingDir = Some(s) })
            | Error _ ->
                { model with
                    GameDir =
                        { model.GameDir with
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
        | Expand(p) ->
            model.Panels
            |> List.map (fun oPanel -> 
                if oPanel.Panel.GetTag = p.Panel.GetTag then
                    { p with Expanded = not p.Expanded }
                else { oPanel with Expanded = false })
            |> fun newPanels ->    
                { model with Panels = newPanels}, Cmd.none
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
        | Submit -> model, Cmd.none