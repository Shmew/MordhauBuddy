namespace MordhauBuddy.App.Settings

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

    let init() =
        { GameDir = 
              { Dir = DirLoad.ConfigFiles(ConfigFile.Game)
                Directory = ""
                Label = "Mordhau Game.ini directory"
                State = DirState.Init "" }
          EngineDir = 
              { Dir = DirLoad.ConfigFiles(ConfigFile.Engine)
                Directory = ""
                Label = "Mordhau Engine.ini directory"
                State = DirState.Init "" }
          GameUserDir =
              { Dir = DirLoad.ConfigFiles(ConfigFile.GameUserSettings)
                Directory = ""
                Label = "Mordhau GameUserSettings.ini directory"
                State = DirState.Init "" }
          MapsDir =
              { Dir = DirLoad.MapDir
                Directory = ""
                Label = "Mordhau maps directory"
                State = DirState.Init "" } }

    let private iniSender = new INIBridgeSender(Caller.Settings)
    let private mapSender = new MapBridgeSender(Caller.Settings)
    let private mapSenderMap = new MapBridgeSender(Caller.MapInstaller)

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
                        | ConfigFile.Game ->
                            { model with
                                GameDir =
                                    { model.GameDir with
                                        State = DirState.Error "Error parsing Engine.ini" }}
                            , Cmd.none
                        | ConfigFile.Engine ->
                            { model with
                                EngineDir =
                                    { model.EngineDir with
                                        State = DirState.Error "Error parsing Engine.ini" }}
                            , Cmd.none
                        | ConfigFile.GameUserSettings ->
                            { model with
                                GameUserDir =
                                    { model.GameUserDir with
                                        State = DirState.Error "Error parsing GameUserSettings.ini" }}
                            , Cmd.none
                    | Some(f), true ->
                        let setGameValid m =
                            { m with GameDir = { m.GameDir with State = DirState.Success "" } }
                        let setGameUserValid m =
                            { m with
                                GameUserDir =
                                    { m.GameUserDir with
                                        State = DirState.Success "" } }
                        let setEngineValid m =
                            { m with
                                EngineDir =
                                    { m.EngineDir with
                                        State = DirState.Success "" } }
                        match f.File with
                        | ConfigFile.Game  ->
                            model |> setGameValid, Cmd.none
                        | ConfigFile.Engine ->
                            model |> setEngineValid, Cmd.none
                        | ConfigFile.GameUserSettings ->
                            model |> setGameUserValid, Cmd.none
                    | _ -> model, Cmd.none
                | INIOperationResult.Exists b ->
                    match bMsg.File with
                    | Some(f) when f.File = ConfigFile.Game ->
                        { model with
                            GameDir =
                                if b then
                                    { model.GameDir with
                                        State = DirState.Success "Game.ini located" } 
                                else
                                    { model.GameDir with
                                        State = DirState.Error "Game.ini not found" } }, 
                        if b then
                            Cmd.bridgeSend
                                (iniSender.Parse { File = ConfigFile.Game; WorkingDir = model.GameDir.Directory |> Some })
                        else Cmd.none
                    | Some(f) when f.File = ConfigFile.Engine ->
                        { model with
                            EngineDir =
                                if b then
                                    { model.EngineDir with
                                        State = DirState.Success "Engine.ini located" } 
                                else
                                    { model.EngineDir with
                                        State = DirState.Error "Engine.ini not found" } }, 
                        if b then
                            Cmd.bridgeSend
                                (iniSender.Parse { File = ConfigFile.Engine; WorkingDir = model.EngineDir.Directory |> Some })
                        else Cmd.none
                    | Some(f) when f.File = ConfigFile.GameUserSettings ->
                        { model with
                            GameUserDir =
                                if b then
                                    { model.GameUserDir with
                                        State = DirState.Success "GameUserSettings.ini located" } 
                                else
                                    { model.GameUserDir with
                                        State = DirState.Error "GameUserSettings.ini not found" } }, 
                        if b then
                            Cmd.bridgeSend 
                                (iniSender.Parse { File = ConfigFile.GameUserSettings; WorkingDir = model.GameUserDir.Directory |> Some })
                        else Cmd.none
                    | _ -> model, Cmd.none
                | INIOperationResult.DefaultDir dOpt ->
                    match dOpt with 
                    | Some(d) ->
                        let mapGame model =
                            { model with
                                GameDir =
                                    { model.GameDir with
                                        Directory = d
                                        State = DirState.Init "Mordhau directory located" } }
                        let mapEngine model =
                            { model with
                                EngineDir =
                                    { model.EngineDir with
                                        Directory = d
                                        State = DirState.Init "Mordhau directory located" } }
                        let mapGameUser model =
                            { model with
                                GameUserDir =
                                    { model.GameUserDir with
                                        Directory = d
                                        State = DirState.Init "Mordhau directory located" } }

                        model
                        |> fun modelWait ->
                            let mList =
                                [ {| IsEmpty = model.GameDir.Directory = ""
                                     Func = mapGame
                                     CmdF = fun m -> 
                                            Cmd.bridgeSend 
                                                (iniSender.Exists 
                                                    { File = ConfigFile.Game
                                                      WorkingDir = Some(m.GameDir.Directory) }) |}
                                  {| IsEmpty = model.EngineDir.Directory = ""
                                     Func = mapEngine
                                     CmdF = fun m -> 
                                             Cmd.bridgeSend 
                                                (iniSender.Exists 
                                                    { File = ConfigFile.Engine
                                                      WorkingDir = Some(m.EngineDir.Directory) }) |}
                                  {| IsEmpty = model.GameUserDir.Directory = ""
                                     Func = mapGameUser
                                     CmdF = fun m -> 
                                            Cmd.bridgeSend 
                                                (iniSender.Exists 
                                                    { File = ConfigFile.GameUserSettings
                                                      WorkingDir = Some(m.GameUserDir.Directory) }) |} ]
                                |> List.filter (fun o -> o.IsEmpty)
                            let m = mList |> List.fold (fun acc o -> acc |> o.Func) modelWait
                            m, Cmd.batch (mList |> List.map (fun o -> m |> o.CmdF))
                    | None ->
                        let mapGame model =
                            { model with
                                GameDir =
                                    { model.GameDir with 
                                        State = DirState.Error "Unable to automatically detect Mordhau directory" } }
                        let mapEngine model =
                            { model with
                                EngineDir =
                                    { model.EngineDir with
                                        State = DirState.Error "Unable to automatically detect Mordhau directory" } }
                        let mapGameUser model =
                            { model with
                                GameUserDir =
                                    { model.GameUserDir with
                                        State = DirState.Error "Unable to automatically detect Mordhau directoryd" } }

                        model
                        |> fun modelWait ->
                            let mList =
                                [ {| IsEmpty = model.GameDir.Directory = ""
                                     Func = mapGame |}
                                  {| IsEmpty = model.GameDir.Directory = ""
                                     Func = mapEngine |}
                                  {| IsEmpty = model.GameDir.Directory = ""
                                     Func = mapGameUser |} ]
                                |> List.filter (fun o -> o.IsEmpty)
                            let m = mList |> List.fold (fun acc o -> acc |> o.Func) modelWait
                            m, Cmd.none
                | _ -> model, Cmd.none
            | BridgeResult.MapOperation mOp ->
                match mOp with
                | MapOperationResult.DefaultDir dOpt ->
                    match dOpt with
                    | Some(d) ->
                        { model with 
                            MapsDir =
                                {model.MapsDir with
                                    Directory = d
                                    State = DirState.Init "Mordhau map directory located" } }
                        |> fun m -> m, Cmd.bridgeSend (mapSender.DirExists(m.MapsDir.Directory))
                    | None ->
                        { model with
                            MapsDir =
                                { model.MapsDir with
                                    State = DirState.Error "Unable to automatically detect Mordhau map directory" } }
                        , Cmd.none
                | MapOperationResult.DirExists b ->
                    if b then
                        { model with
                            MapsDir =
                                { model.MapsDir with
                                    State = DirState.Success "Maps directory located" } }
                        , Cmd.bridgeSend (mapSenderMap.GetInstalled(model.MapsDir.Directory))
                    else
                        { model with
                            MapsDir =
                                { model.MapsDir with
                                    State = DirState.Error "Maps directory not found" } }
                        , Cmd.none
                | _ -> model, Cmd.none
            | _ -> model, Cmd.none
        | GetDefaultDir ->
            model, Cmd.bridgeSend (iniSender.DefaultDir)
        | GetMapDir ->
            model, Cmd.bridgeSend (mapSender.DefaultDir)
        | SetConfigDir (s,res,cFile) ->
            match cFile with
            | ConfigFile.Game ->
                match res with
                | Ok s ->
                    { model with
                        GameDir =
                            { model.GameDir with
                                Directory = s
                                State = DirState.Init "" } }
                    , Cmd.bridgeSend (iniSender.Exists <|
                        { File = cFile
                          WorkingDir = Some(s) })
                | Error _ ->
                    { model with
                        GameDir =
                            { model.GameDir with
                                Directory = s
                                State = DirState.Error <| errorStrings res } }
                    , Cmd.none
            | ConfigFile.Engine ->
                match res with
                | Ok s ->
                    { model with
                        EngineDir =
                            { model.EngineDir with
                                Directory = s
                                State = DirState.Init "" } }
                    , Cmd.bridgeSend (iniSender.Exists <|
                        { File = cFile
                          WorkingDir = Some(s) })
                | Error _ ->
                    { model with
                        EngineDir =
                            { model.EngineDir with
                                Directory = s
                                State = DirState.Error <| errorStrings res } }
                    , Cmd.none
            | ConfigFile.GameUserSettings ->
                match res with
                | Ok s ->
                    { model with
                        GameUserDir =
                            { model.GameUserDir with
                                Directory = s
                                State = DirState.Init "" } }
                    , Cmd.bridgeSend (iniSender.Exists <|
                        { File = cFile
                          WorkingDir = Some(s) })
                | Error _ ->
                    { model with
                        GameUserDir =
                            { model.GameUserDir with
                                Directory = s
                                State = DirState.Error <| errorStrings res } }
                    , Cmd.none
        | SetMapDir (s,res) ->
            match res with
            | Ok s ->
                { model with
                    MapsDir =
                        { model.MapsDir with
                            Directory = s
                            State = DirState.Init "" } }
                , Cmd.bridgeSend (mapSender.DirExists s)
            | Error _ ->
                { model with
                    GameUserDir =
                        { model.GameUserDir with
                            Directory = s
                            State = DirState.Error <| errorStrings res } }
                , Cmd.none
        | RequestLoad dirLoad ->
            let handleLoaded =
                match dirLoad with
                | ConfigFiles(cFile) ->
                    function
                    | DirSelect.Selected s ->
                        (s, validateDir s, cFile)
                        |> SetConfigDir
                    | DirSelect.Canceled -> LoadCanceled
                | MapDir ->
                    function
                    | DirSelect.Selected s ->
                        (s, validateDir s)
                        |> SetMapDir
                    | DirSelect.Canceled -> LoadCanceled
            model, Cmd.OfPromise.perform selectDir () handleLoaded
        | LoadCanceled -> model, Cmd.none
