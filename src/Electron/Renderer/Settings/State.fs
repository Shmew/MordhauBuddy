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
        { Waiting = true
          Complete = false
          GameDir = 
              { Dir = DirLoad.ConfigFiles(ConfigFile.Game)
                Waiting = false
                Directory = ""
                Error = false
                Label = "Mordhau Game.ini directory"
                HelperText = "" 
                Validated = false }
          EngineDir = 
              { Dir = DirLoad.ConfigFiles(ConfigFile.Engine)
                Waiting = false
                Directory = ""
                Error = false
                Label = "Mordhau Engine.ini directory"
                HelperText = "" 
                Validated = false }
          GameUserDir =
              { Dir = DirLoad.ConfigFiles(ConfigFile.GameUserSettings)
                Waiting = false
                Directory = ""
                Error = false
                Label = "Mordhau GameUserSettings.ini directory"
                HelperText = "" 
                Validated = false }
          MapsDir =
              { Dir = DirLoad.MapDir
                Waiting = false
                Directory = ""
                Error = false
                Label = "Mordhau maps directory"
                HelperText = "" 
                Validated = false } }

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
                                Waiting = false
                                GameDir =
                                    { model.GameDir with
                                        Waiting = false
                                        Error = true
                                        HelperText = "Error parsing Engine.ini" }}
                            , Cmd.none
                        | ConfigFile.Engine ->
                            { model with
                                Waiting = false
                                EngineDir =
                                    { model.EngineDir with
                                        Waiting = false
                                        Error = true
                                        HelperText = "Error parsing Engine.ini" }}
                            , Cmd.none
                        | ConfigFile.GameUserSettings ->
                            { model with
                                Waiting = false
                                GameUserDir =
                                    { model.GameUserDir with
                                        Waiting = false
                                        Error = true
                                        HelperText = "Error parsing GameUserSettings.ini" }}
                            , Cmd.none
                    | Some(f), true ->
                        let setGameValid m =
                            { m with
                                GameDir =
                                    { m.GameDir with
                                        Waiting = false
                                        Validated = true } }
                        let setGameUserValid m =
                            { m with
                                GameUserDir =
                                    { m.GameUserDir with
                                        Waiting = false
                                        Validated = true } }
                        let setEngineValid m =
                            { m with
                                EngineDir =
                                    { m.EngineDir with
                                        Waiting = false
                                        Validated = true } }
                        match f.File with
                        | ConfigFile.Game  ->
                            model |> setGameValid, Cmd.none
                        | ConfigFile.Engine ->
                            model |> setEngineValid, Cmd.none
                        | ConfigFile.GameUserSettings ->
                            model |> setGameUserValid, Cmd.none
                    | _ -> { model with Waiting = false }, Cmd.none
                | INIOperationResult.Exists b ->
                    match bMsg.File with
                    | Some(f) when f.File = ConfigFile.Game ->
                        { model with
                            Waiting = false
                            GameDir =
                                if b then
                                    { model.GameDir with
                                        Waiting = true
                                        Error = false
                                        HelperText = "Game.ini located"
                                        Validated = true } 
                                else
                                    { model.GameDir with
                                        Error = true
                                        HelperText = "Game.ini not found"
                                        Validated = false } }, 
                        if b then
                            Cmd.bridgeSend
                                (iniSender.Parse { File = ConfigFile.Game; WorkingDir = model.GameDir.Directory |> Some })
                        else Cmd.none
                    | Some(f) when f.File = ConfigFile.Engine ->
                        { model with
                            Waiting = false
                            EngineDir =
                                if b then
                                    { model.EngineDir with
                                        Waiting = true
                                        Error = false
                                        HelperText = "Engine.ini located"
                                        Validated = true } 
                                else
                                    { model.EngineDir with
                                        Error = true
                                        HelperText = "Engine.ini not found"
                                        Validated = false } }, 
                        if b then
                            Cmd.bridgeSend
                                (iniSender.Parse { File = ConfigFile.Engine; WorkingDir = model.EngineDir.Directory |> Some })
                        else Cmd.none
                    | Some(f) when f.File = ConfigFile.GameUserSettings ->
                        { model with
                            Waiting = false
                            GameUserDir =
                                if b then
                                    { model.GameUserDir with
                                        Waiting = true
                                        Error = false
                                        HelperText = "GameUserSettings.ini located"
                                        Validated = true } 
                                else
                                    { model.GameUserDir with
                                        Error = true
                                        HelperText = "GameUserSettings.ini not found"
                                        Validated = false } }, 
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
                                        HelperText = "Mordhau directory located"
                                        Validated = false } }
                        let mapEngine model =
                            { model with
                                EngineDir =
                                    { model.EngineDir with
                                        Directory = d 
                                        HelperText = "Mordhau directory located"
                                        Validated = false } }
                        let mapGameUser model =
                            { model with
                                GameUserDir =
                                    { model.GameUserDir with
                                        Directory = d 
                                        HelperText = "Mordhau directory located"
                                        Validated = false } }

                        { model with Waiting = false}
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
                                        Waiting = false
                                        Error = true
                                        HelperText = "Unable to automatically detect Mordhau directory"
                                        Validated = false } }
                        let mapEngine model =
                            { model with
                                EngineDir =
                                    { model.EngineDir with
                                        Waiting = false
                                        Error = true
                                        HelperText = "Unable to automatically detect Mordhau directory"
                                        Validated = false } }
                        let mapGameUser model =
                            { model with
                                GameUserDir =
                                    { model.GameUserDir with
                                        Waiting = false
                                        Error = true
                                        HelperText = "Unable to automatically detect Mordhau directoryd"
                                        Validated = false } }

                        { model with Waiting = false}
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
                            Waiting = false
                            MapsDir =
                                {model.MapsDir with
                                    Directory = d
                                    HelperText = "Mordhau map directory located"
                                    Validated = false } }
                        |> fun m -> m, Cmd.bridgeSend (mapSender.DirExists(m.MapsDir.Directory))
                    | None ->
                        { model with
                            Waiting = false
                            MapsDir =
                                { model.MapsDir with
                                    Error = true
                                    HelperText = "Unable to automatically detect Mordhau map directory"
                                    Validated = false } }, Cmd.none
                | MapOperationResult.DirExists b ->
                    if b then
                        { model with
                            Waiting = false 
                            MapsDir =
                                { model.MapsDir with
                                    Waiting = false
                                    Error = false
                                    HelperText = "Maps directory located"
                                    Validated = true } }, Cmd.bridgeSend (mapSenderMap.GetInstalled(model.MapsDir.Directory))
                    else
                        { model with
                            Waiting = false 
                            MapsDir =
                                { model.MapsDir with
                                    Waiting = false
                                    Error = true
                                    HelperText = "Maps directory not found"
                                    Validated = false } }, Cmd.none
                | _ -> { model with Waiting = false }, Cmd.none
            | _ -> { model with Waiting = false }, Cmd.none
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
                                Error = false
                                HelperText = "" } }
                    , Cmd.bridgeSend (iniSender.Exists <|
                        { File = cFile
                          WorkingDir = Some(s) })
                | Error _ ->
                    { model with
                        GameDir =
                            { model.GameDir with
                                Directory = s
                                Error = true
                                HelperText = errorStrings res } }
                    , Cmd.none
            | ConfigFile.Engine ->
                match res with
                | Ok s ->
                    { model with
                        EngineDir =
                            { model.EngineDir with
                                Directory = s
                                Error = false
                                HelperText = "" } }
                    , Cmd.bridgeSend (iniSender.Exists <|
                        { File = cFile
                          WorkingDir = Some(s) })
                | Error _ ->
                    { model with
                        EngineDir =
                            { model.EngineDir with
                                Directory = s
                                Error = true
                                HelperText = errorStrings res } }
                    , Cmd.none
            | ConfigFile.GameUserSettings ->
                match res with
                | Ok s ->
                    { model with
                        GameUserDir =
                            { model.GameUserDir with
                                Directory = s
                                Error = false
                                HelperText = "" } }
                    , Cmd.bridgeSend (iniSender.Exists <|
                        { File = cFile
                          WorkingDir = Some(s) })
                | Error _ ->
                    { model with
                        GameUserDir =
                            { model.GameUserDir with
                                Directory = s
                                Error = true
                                HelperText = errorStrings res } }
                    , Cmd.none
        | SetMapDir (s,res) ->
            match res with
            | Ok s ->
                { model with
                    MapsDir =
                        { model.MapsDir with
                            Directory = s
                            Error = false
                            HelperText = "" } }
                , Cmd.bridgeSend (mapSender.DirExists s)
            | Error _ ->
                { model with
                    GameUserDir =
                        { model.GameUserDir with
                            Directory = s
                            Error = true
                            HelperText = errorStrings res } }
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
