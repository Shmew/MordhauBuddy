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

    [<AutoOpen>]
    module private Helpers =
        let iniSender = new INIBridgeSender(Caller.Settings)
        let mapSender = new MapBridgeSender(Caller.Settings)
        let mapSenderMap = new MapBridgeSender(Caller.MapInstaller)

        type Dirs =
            | Game
            | Engine
            | GameUser
            | Maps

        let setDirError s (dir: ConfigDir) = { dir with State = DirState.Error s }

        let setDirSuccess s (dir: ConfigDir) = { dir with State = DirState.Success s }

        let setDirInit s (dir: ConfigDir) = { dir with State = DirState.Init s }

        let setDirDirectory s (dir: ConfigDir) = { dir with Directory = s }

        let iFileWithDir (cFile: ConfigFile) (dir: string) =
            { File = cFile
              WorkingDir = dir |> Some }

        let setDir (model: Model) (dirType: Dirs) (dir: ConfigDir) =
            match dirType with
            | Game -> { model with GameDir = dir }
            | Engine -> { model with EngineDir = dir }
            | GameUser -> { model with GameUserDir = dir }
            | Maps -> { model with MapsDir = dir }

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
            |> iniSender.Parse
            |> Cmd.bridgeSend

        let sendParseIf (cFile: ConfigFile) (dir: string) (b: bool) =
            if b then sendParse cFile dir
            else Cmd.none

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
                            setDir model GameUser <| setDirError "Error parsing GameUserSettings.ini" model.GameUserDir
                        |> fun m -> m, Cmd.none
                    | Some(f), true ->
                        match f.File with
                        | ConfigFile.Game -> setDir model Game <| setDirSuccess "" model.GameDir
                        | ConfigFile.Engine -> setDir model Engine <| setDirSuccess "" model.EngineDir
                        | ConfigFile.GameUserSettings -> setDir model GameUser <| setDirSuccess "" model.GameUserDir
                        |> fun m -> m, Cmd.none
                    | _ -> model, Cmd.none
                | INIOperationResult.Exists b ->
                    match bMsg.File with
                    | Some(f) ->
                        match f.File with
                        | ConfigFile.Game ->
                            if b then setDirSuccess "Game.ini located" model.GameDir
                            else setDirError "Game.ini not found" model.GameDir
                            |> fun cDir -> setDir model Game cDir, model.GameDir.Directory
                        | ConfigFile.Engine ->
                            if b then setDirSuccess "Engine.ini located" model.EngineDir
                            else setDirError "Engine.ini not found" model.EngineDir
                            |> fun cDir -> setDir model Engine cDir, model.EngineDir.Directory
                        | ConfigFile.GameUserSettings ->
                            if b then setDirSuccess "GameUserSettings.ini located" model.GameUserDir
                            else setDirError "GameUserSettings.ini not found" model.GameUserDir
                            |> fun cDir -> setDir model GameUser cDir, model.GameUserDir.Directory
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
                                 CmdF = fun m -> sendParse ConfigFile.GameUserSettings m.GameUserDir.Directory |} ]
                            |> List.filter (fun o -> o.IsEmpty)

                        let m = mList |> List.fold (fun acc o -> acc |> o.Func) model
                        m, Cmd.batch (mList |> List.map (fun o -> m |> o.CmdF))
                    | None ->
                        let errMsg = "Unable to automatically detect Mordhau directory"

                        let mList =
                            [ {| IsEmpty = model.GameDir.Directory = ""
                                 Func = fun m -> setDirError errMsg m.GameDir |> setDir m Game |}
                              {| IsEmpty = model.GameDir.Directory = ""
                                 Func = fun m -> setDirError errMsg m.EngineDir |> setDir m Engine |}
                              {| IsEmpty = model.GameDir.Directory = ""
                                 Func = fun m -> setDirError errMsg m.GameUserDir |> setDir m GameUser |} ]
                            |> List.filter (fun o -> o.IsEmpty)

                        let m = mList |> List.fold (fun acc o -> acc |> o.Func) model
                        m, Cmd.none
                | _ -> model, Cmd.none
            | BridgeResult.MapOperation mOp ->
                match mOp with
                | MapOperationResult.DefaultDir dOpt ->
                    match dOpt with
                    | Some(d) ->
                        setDirInit "Mordhau map directory located" model.MapsDir
                        |> setDirDirectory d
                        |> setDir model Maps
                        |> fun m ->
                            m,
                            m.MapsDir.Directory
                            |> mapSender.DirExists
                            |> Cmd.bridgeSend
                    | None ->
                        setDirError "Unable to automatically detect Mordhau map directory" model.MapsDir
                        |> setDir model Maps
                        |> fun m -> m, Cmd.none
                | MapOperationResult.DirExists b ->
                    if b then
                        setDirSuccess "Maps directory located" model.MapsDir
                        |> setDir model Maps
                        |> fun m ->
                            m,
                            model.MapsDir.Directory
                            |> mapSenderMap.GetInstalled
                            |> Cmd.bridgeSend
                    else
                        setDirSuccess "Maps directory not found" model.MapsDir
                        |> setDir model Maps
                        |> fun m -> m, Cmd.none
                | _ -> model, Cmd.none
            | _ -> model, Cmd.none
        | GetDefaultDir -> model, iniSender.DefaultDir |> Cmd.bridgeSend
        | GetMapDir -> model, mapSender.DefaultDir |> Cmd.bridgeSend
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
            |> fun (d, c) -> setConfigDir res c d
        | SetMapDir(s, res) ->
            match res with
            | Ok s ->
                setDirInit "" model.MapsDir
                |> setDirDirectory s
                |> setDir model Maps
                |> fun m -> m, Cmd.bridgeSend (mapSender.DirExists s)
            | Error _ ->
                errorStrings res
                |> setDirError
                <| model.MapsDir
                |> setDirDirectory s
                |> setDir model Maps
                |> fun m -> m, Cmd.none
        | RequestLoad dirLoad ->
            let handleLoaded =
                match dirLoad with
                | ConfigFiles(cFile) ->
                    function
                    | DirSelect.Selected s -> (s, validateDir s, cFile) |> SetConfigDir
                    | DirSelect.Canceled -> LoadCanceled
                | MapDir ->
                    function
                    | DirSelect.Selected s -> (s, validateDir s) |> SetMapDir
                    | DirSelect.Canceled -> LoadCanceled
            model, Cmd.OfPromise.perform selectDir () handleLoaded
        | LoadCanceled -> model, Cmd.none
