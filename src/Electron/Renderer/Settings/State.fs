namespace MordhauBuddy.App.Settings

module State =
    open FSharp.Core // To avoid shadowing Result<_,_>
    open MordhauBuddy.App
    open RenderUtils
    open RenderUtils.Validation
    open Elmish
    open Elmish.Bridge
    open MordhauBuddy.Shared.ElectronBridge
    open BridgeUtils
    open RenderUtils.Directory
    open Types
    open Node.Api

    let init (uSet: UpdateSettings, bSet: BackupSettings, autoL: bool) =
        { GameDir =
              { Dir = DirLoad.ConfigFiles ConfigFile.Game
                Directory = ""
                Label = "Mordhau Game.ini directory"
                State = DirState.Init "" }
          EngineDir =
              { Dir = DirLoad.ConfigFiles ConfigFile.Engine
                Directory = ""
                Label = "Mordhau Engine.ini directory"
                State = DirState.Init "" }
          GameUserDir =
              { Dir = DirLoad.ConfigFiles ConfigFile.GameUserSettings
                Directory = ""
                Label = "Mordhau GameUserSettings.ini directory"
                State = DirState.Init "" }
          InputDir =
              { Dir = DirLoad.ConfigFiles ConfigFile.Input
                Directory = ""
                Label = "Mordhau Input.ini directory"
                State = DirState.Init "" }
          ModsDir =
              { Dir = DirLoad.ModDir
                Directory = ""
                Label = "Mordhau mods directory"
                State = DirState.Init "" }
          ModUpdateSettings = uSet
          BackupSettings = bSet
          AutoLaunch = autoL }

    [<AutoOpen>]
    module private Helpers =
        let iniSender = new INIBridgeSender(Caller.Settings)
        let modSender = new ModBridgeSender(Caller.Settings)
        let modSenderMod = new ModBridgeSender(Caller.ModInstaller)
        let settingsSender = new SettingBridgeSender(Caller.Settings)

        let setDir (model: Model) (dirType: Dirs) (dir: ConfigDir) =
            match dirType with
            | Game -> { model with GameDir = dir }
            | Engine -> { model with EngineDir = dir }
            | GameUser -> { model with GameUserDir = dir }
            | Input -> { model with InputDir = dir }
            | Mods -> { model with ModsDir = dir }

        let iFileWithDir (cFile: ConfigFile) (dir: string) =
            { File = cFile
              WorkingDir = dir |> Some }

        let sendParse (cFile: ConfigFile) (dir: string) =
            match cFile with
            | ConfigFile.Game ->
                { File = ConfigFile.Game
                  WorkingDir = dir |> Some }
            | ConfigFile.Engine ->
                { File = ConfigFile.Engine
                  WorkingDir = dir |> Some }
            | ConfigFile.GameUserSettings ->
                { File = ConfigFile.GameUserSettings
                  WorkingDir = dir |> Some }
            | ConfigFile.Input ->
                { File = ConfigFile.Input
                  WorkingDir = dir |> Some }
            |> iniSender.Parse
            |> Cmd.bridgeSend

        let sendParseIf (cFile: ConfigFile) (dir: string) (b: bool) =
            if b then sendParse cFile dir else Cmd.none

    let update (msg: Msg) (model: Model) =
        match msg with
        | ClientMsg bMsg ->
            match bMsg.BridgeResult with
            | BridgeResult.INIOperation iOp ->
                match iOp with
                | INIOperationResult.Parse b ->
                    match bMsg.File, b with
                    | Some(f), false ->
                        match f.File with
                        | ConfigFile.Game -> setDir model Game <| setDirError "Error parsing Game.ini" model.GameDir
                        | ConfigFile.Engine ->
                            setDir model Engine <| setDirError "Error parsing Engine.ini" model.EngineDir
                        | ConfigFile.GameUserSettings ->
                            setDir model GameUser
                            <| setDirError "Error parsing GameUserSettings.ini" model.GameUserDir
                        | ConfigFile.Input -> setDir model Input <| setDirError "Error parsing Input.ini" model.InputDir
                        |> fun m -> m, Cmd.none
                    | Some(f), true ->
                        match f.File with
                        | ConfigFile.Game -> setDir model Game <| setDirSuccess "" model.GameDir
                        | ConfigFile.Engine -> setDir model Engine <| setDirSuccess "" model.EngineDir
                        | ConfigFile.GameUserSettings -> setDir model GameUser <| setDirSuccess "" model.GameUserDir
                        | ConfigFile.Input -> setDir model Input <| setDirSuccess "" model.InputDir
                        |> fun m -> m, Cmd.none
                    | _ -> model, Cmd.none
                | INIOperationResult.Exists b ->
                    let setNotFound s =
                        sprintf "%s.ini not found. Usually located at a path like:\n%s" s
                            RenderUtils.Samples.typicalConfigDir
                    match bMsg.File with
                    | Some(f) ->
                        match f.File with
                        | ConfigFile.Game ->
                            if b
                            then setDirSuccess "Game.ini located" model.GameDir
                            else setDirError (setNotFound "Game") model.GameDir
                            |> fun cDir -> setDir model Game cDir, model.GameDir.Directory
                        | ConfigFile.Engine ->
                            if b
                            then setDirSuccess "Engine.ini located" model.EngineDir
                            else setDirError (setNotFound "Engine") model.EngineDir
                            |> fun cDir -> setDir model Engine cDir, model.EngineDir.Directory
                        | ConfigFile.GameUserSettings ->
                            if b
                            then setDirSuccess "GameUserSettings.ini located" model.GameUserDir
                            else setDirError (setNotFound "GameUserSettings") model.GameUserDir
                            |> fun cDir -> setDir model GameUser cDir, model.GameUserDir.Directory
                        | ConfigFile.Input ->
                            if b
                            then setDirSuccess "Input.ini located" model.InputDir
                            else setDirError (setNotFound "Input") model.InputDir
                            |> fun cDir -> setDir model Input cDir, model.InputDir.Directory
                        |> fun (m, dir) -> m, sendParseIf f.File dir b
                    | _ -> model, Cmd.none
                | INIOperationResult.DefaultDir dOpt ->
                    match dOpt with
                    | Some(d) ->
                        let initMsg = "Mordhau directory located"

                        let mList =
                            [ {| IsEmpty = model.GameDir.Directory = ""
                                 Func =
                                     fun m ->
                                         setDirInit initMsg m.GameDir
                                         |> setDirDirectory d
                                         |> setDir m Game
                                 CmdF = fun m -> sendParse ConfigFile.Game m.GameDir.Directory |}
                              {| IsEmpty = model.EngineDir.Directory = ""
                                 Func =
                                     fun m ->
                                         setDirInit initMsg m.EngineDir
                                         |> setDirDirectory d
                                         |> setDir m Engine
                                 CmdF = fun m -> sendParse ConfigFile.Engine m.EngineDir.Directory |}
                              {| IsEmpty = model.GameUserDir.Directory = ""
                                 Func =
                                     fun m ->
                                         setDirInit initMsg m.GameUserDir
                                         |> setDirDirectory d
                                         |> setDir m GameUser
                                 CmdF = fun m -> sendParse ConfigFile.GameUserSettings m.GameUserDir.Directory |}
                              {| IsEmpty = model.InputDir.Directory = ""
                                 Func =
                                     fun m ->
                                         setDirInit initMsg m.InputDir
                                         |> setDirDirectory d
                                         |> setDir m Input
                                 CmdF = fun m -> sendParse ConfigFile.Input m.InputDir.Directory |}]
                            |> List.filter (fun o -> o.IsEmpty)

                        let m = mList |> List.fold (fun acc o -> acc |> o.Func) model
                        m, Cmd.batch (mList |> List.map (fun o -> m |> o.CmdF))
                    | None ->
                        let errMsg = "Unable to automatically detect Mordhau directory"

                        let mList =
                            [ {| IsEmpty = model.GameDir.Directory = ""
                                 Func = fun m -> setDirError errMsg m.GameDir |> setDir m Game |}
                              {| IsEmpty = model.EngineDir.Directory = ""
                                 Func = fun m -> setDirError errMsg m.EngineDir |> setDir m Engine |}
                              {| IsEmpty = model.GameUserDir.Directory = ""
                                 Func = fun m -> setDirError errMsg m.GameUserDir |> setDir m GameUser |}
                              {| IsEmpty = model.InputDir.Directory = ""
                                 Func = fun m -> setDirError errMsg m.InputDir |> setDir m Input |} ]
                            |> List.filter (fun o -> o.IsEmpty)

                        let m = mList |> List.fold (fun acc o -> acc |> o.Func) model
                        m, Cmd.none
                | _ -> model, Cmd.none
            | BridgeResult.ModOperation mOp ->
                match mOp with
                | ModOperationResult.DefaultDir dOpt ->
                    match dOpt with
                    | Some(d) ->
                        setDirInit "Mordhau mod directory located" model.ModsDir
                        |> setDirDirectory d
                        |> setDir model Mods
                        |> fun m ->
                            m,
                            m.ModsDir.Directory
                            |> modSender.DirExists
                            |> Cmd.bridgeSend
                    | None ->
                        setDirError "Unable to automatically detect Mordhau map directory" model.ModsDir
                        |> setDir model Mods
                        |> fun m -> m, Cmd.none
                | ModOperationResult.DirExists b ->
                    if b then
                        setDirSuccess "" model.ModsDir
                        |> setDir model Mods
                        |> fun m ->
                            m,
                            model.ModsDir.Directory
                            |> modSenderMod.GetInstalled
                            |> Cmd.bridgeSend
                    else
                        setDirError "Mods directory not found" model.ModsDir
                        |> setDir model Mods
                        |> fun m -> m, Cmd.none
                | _ -> model, Cmd.none
            | BridgeResult.Settings sOp ->
                match sOp with
                | SettingResult.EnabledAutoLaunch b -> { model with AutoLaunch = b }, Cmd.none
                | SettingResult.DisabledAutoLaunch b -> { model with AutoLaunch = b |> not }, Cmd.none
            | _ -> model, Cmd.none
        | GetDefaultDir -> model, iniSender.DefaultDir |> Cmd.bridgeSend
        | GetModDir -> model, modSender.DefaultDir |> Cmd.bridgeSend
        | SetConfigDir(s, res, cFile) ->
            let setConfigDir (res: Result<string, string list>) (cDir: ConfigDir) (dirType: Dirs) =
                match res with
                | Ok s ->
                    setDirInit "" cDir
                    |> setDirDirectory s
                    |> setDir model dirType
                    |> fun m ->
                        m,
                        iFileWithDir cFile s
                        |> iniSender.Exists
                        |> Cmd.bridgeSend
                | Error _ ->
                    res
                    |> errorStrings
                    |> setDirError
                    <| cDir
                    |> setDirDirectory s
                    |> setDir model dirType
                    |> fun m -> m, Cmd.none
            match cFile with
            | ConfigFile.Game -> Game, model.GameDir
            | ConfigFile.Engine -> Engine, model.EngineDir
            | ConfigFile.GameUserSettings -> GameUser, model.GameUserDir
            | ConfigFile.Input -> Input, model.InputDir
            |> fun (d, c) -> setConfigDir res c d
        | SetModDir(s, res) ->
            match res with
            | Ok s ->
                setDirInit "" model.ModsDir
                |> setDirDirectory s
                |> setDir model Mods
                |> fun m -> m, Cmd.bridgeSend (modSender.DirExists s)
            | Error _ ->
                errorStrings res
                |> setDirError
                <| model.ModsDir
                |> setDirDirectory s
                |> setDir model Mods
                |> fun m -> m, Cmd.none
        | RequestLoad dirLoad ->
            let handleLoaded =
                match dirLoad with
                | ConfigFiles(cFile) ->
                    function
                    | DirSelect.Selected s -> (s, validateDir s, cFile) |> SetConfigDir
                    | DirSelect.Canceled -> LoadCanceled
                | ModDir ->
                    function
                    | DirSelect.Selected s -> (s, validateDir s) |> SetModDir
                    | DirSelect.Canceled -> LoadCanceled
            model, Cmd.OfPromise.perform selectDir () handleLoaded
        | LoadCanceled -> model, Cmd.none
        | ModUpdateSettings newSets ->
            match newSets with
            | Some(s) -> { model with ModUpdateSettings = s }, Cmd.none
            | _ -> model, Cmd.none
        | BackupSetting newSets ->
            match newSets with
            | Some(s) -> { model with BackupSettings = s }, Cmd.bridgeSend (settingsSender.BackupPolicy(s))
            | _ -> model, Cmd.none
        | ToggleAutoLaunch ->
            if model.AutoLaunch then settingsSender.DisableAutoLaunch else settingsSender.EnableAutoLaunch
            |> fun sender -> { model with AutoLaunch = model.AutoLaunch |> not }, Cmd.bridgeSend sender
        | RunSetup ->
            let cmds =
                [ yield if model.AutoLaunch then settingsSender.EnableAutoLaunch
                        else settingsSender.DisableAutoLaunch
                        |> Cmd.bridgeSend
                  if ``process``.platform <> Node.Base.Platform.Win32 then
                      yield Cmd.bridgeSend (settingsSender.SetupLinux)
                  if ``process``.platform <> Node.Base.Platform.Win32 then
                    yield Cmd.bridgeSend (settingsSender.SetupLinux) ]
            model, Cmd.batch cmds
