namespace MordhauBuddy.App

module BridgeUtils =
    open MordhauBuddy.Shared.ElectronBridge
    
    [<RequireQualifiedAccess>]
    module INI =
        [<RequireQualifiedAccess>]
        module Ops =
            let private wrapOps iCmd = INIOps(Operation(iCmd))
            let replace s sels = Replace(s,sels) |> wrapOps
            let delete sels = Delete(sels) |> wrapOps
            let exists iFile = Exists(iFile) |> wrapOps
            let parse iFile = Parse(iFile) |> wrapOps
            let backup iFile = Backup(iFile) |> wrapOps
            let defDir = DefaultDir |> wrapOps
            let commit iFile = Commit(iFile) |> wrapOps

        [<RequireQualifiedAccess>]
        module Faces =
            let private wrapFace fCmd = INIOps(Faces(fCmd))
            let setRandom profile = Random(profile) |> wrapFace
            let setFrankenstein profile = Frankenstein(profile) |> wrapFace
            let setCustom profile fVal = Custom(profile,fVal) |> wrapFace
            let getProfileList = ProfileList |> wrapFace

module RenderUtils =
    open Electron
    open Fable.Core
    open Fable.Core.JsInterop
    open Fable.Import
    open Browser.Types
    
    let isWindows = Node.Api.``process``.platform = Node.Base.Platform.Win32
    
    let getRemoteWin() = renderer.remote.getCurrentWindow()

    [<Emit("$0.persist()")>]
    let eventPersist (e: Event) : unit = jsNative

    [<Emit("__static + \"/\" + $0")>]
    let private stat' (s : string) : string = jsNative

    [<Emit("document.execCommand(\"Cut\")")>]
    let private cut () : unit = jsNative

    [<Emit("document.execCommand(\"Copy\")")>]
    let private copy () : unit = jsNative

    [<Emit("document.execCommand(\"Paste\")")>]
    let private paste () : unit = jsNative

    [<Emit("document.execCommand(\"SelectAll\")")>]
    let private selectAll () : unit = jsNative

    [<Emit("try{document.elementFromPoint($0, $1)}catch(e){}")>]
    let getElementAtPos (x: int) (y: int) : HTMLElement option = jsNative
    
    let getMousePositions () = 
        let absMouse = renderer.remote.screen.getCursorScreenPoint()
        let absWindow = renderer.remote.getCurrentWindow().getBounds()
        {| X = absMouse.x - absWindow.x; Y = absMouse.y - absWindow.y |}

    /// Prefixes the string with the static asset root path.
    let stat (s : string) =
#if DEBUG
        s
#else
        stat' s
#endif

    module String =
        open System.Text.RegularExpressions

        let ensureEndsWith (suffix : string) (str : string) =
            if str.EndsWith suffix then str
            else str + suffix

        let duToTitle (s : string) =
            MatchEvaluator(fun m -> " " + m.Value)
            |> (fun m -> Regex.Replace(s.Substring(1), "[A-Z]", m))
            |> (+) (s.Substring(0, 1))

    module ElectronStore =
        type Store =
            abstract set : string * string -> unit
            abstract set : obj -> unit
            abstract get : string * ?defaultValue:string -> obj
            abstract has : string -> bool
            abstract delete : string -> unit
            abstract clear : unit
            abstract onDidChange : string * Browser.Types.Event -> unit
            abstract onDidAnyChange : Browser.Types.Event -> unit
            abstract size : int
            abstract store : obj
            abstract path : string
            abstract openInEditor : unit

        type StoreStatic =
            [<EmitConstructor>]
            abstract Create : unit -> Store

        let getStore : StoreStatic = importDefault "electron-store"

        let store = getStore.Create()

    [<AutoOpen>]
    module Extensions =
        type Result<'T, 'TError> with

            member this.IsOk =
                match this with
                | Ok _ -> true
                | Error _ -> false

            member this.IsError =
                match this with
                | Error _ -> true
                | Ok _ -> false

            member this.ErrorOr value =
                match this with
                | Ok _ -> value
                | Error err -> err

        let errorStrings (res: Result<string,string list>) =
            match res with
            | Ok (s: string) -> s
            | Error (err: string list) -> 
                err |> List.reduce (fun acc elem -> acc + " " + elem)

    module Validation =
        open Fable.Core.Testing
        open Fable.Validation.Core
        open Node.Api
        open System.Text.RegularExpressions

        let validateConfigDir (s: string) = Fable.Validation.Core.single <| fun t ->
            t.TestOne s
                |> t.IsValid (fun _ -> 
                    try 
                        s |> path.normalize |> path.parse |> ignore
                        true
                    with 
                    | _ -> false) "Invalid path"
                |> t.Trim
                |> t.NotBlank "Directory cannot be blank"
                |> t.End

        let validateImport (s: string) = Fable.Validation.Core.single <| fun t ->
            t.TestOne s
                |> t.Trim
                |> t.NotBlank "Must provide import string"
                |> t.Match (Regex @"^\(Translate=\((\d*,?){49}\),Rotate=\((\d*,?){49}\),Scale=\((\d*,?){49}\)\)") "Invalid import string"
                |> t.End

    module Directory =
        [<RequireQualifiedAccess>]
        type DirSelect =
            | Selected of string
            | Canceled

        let selectDir () =
            promise {
                let opts = jsOptions<OpenDialogOptions>(fun o ->
                    // See https://github.com/electron/electron/blob/master/docs/api/dialog.md
                    o.title <- "Select Mordhau Configuration Directory"
                    o.defaultPath <- renderer.remote.app.getPath AppPathName.Home
                    o.properties <- [| DialogFeature.OpenDirectory |]
                )
                let! res = renderer.remote.dialog.showOpenDialog opts
                if res.canceled then return DirSelect.Canceled
                else
                    return res.filePaths |> Array.head |> DirSelect.Selected
            }
            //return 
            //if isWindows then res.filePaths |> Array.reduce (fun acc elem -> acc + @"\" + elem)
            //else res.filePaths |> Array.reduce (fun acc elem -> acc + "/" + elem)
            //|> DirSelect.Selected

    module Samples =
        let faceImport =
            "(Translate=(65535,31072,875,704,0,734,65535,0,0,65535,31565,0,0,65535,0,29632,29662,30686,\
            65535,65535,0,30720,65535,0,0,918,31560,0,65535,31709,31680,544,574,30749,30720,256,286,65535,\
            0,0,0,65535,0,0,65535,0,65535,31678,31648),Rotate=(0,65535,26302,31680,0,30750,0,65535,0,0,0,\
            65535,65535,65535,31584,30749,31648,8,65535,0,65535,608,65535,65535,0,31695,893,18301,65535,31677,\
            30720,31704,30725,1,988,29,960,0,65535,0,65535,65535,16326,0,65535,65535,15383,30,960),Scale=(0,30,\
            4139,30749,65535,30749,0,65535,65535,0,0,0,0,65535,31709,0,0,190,0,0,0,589,0,0,0,30749,31166,989,\
            65535,5085,5085,4242,4242,0,0,24452,24452,65535,0,0,65535,65535,574,0,0,65535,574,21470,21470))"

    module AppContextMenu =
        open Fable.React
        open Fable.React.Props
        open Fable.MaterialUI.Core
        open Fable.MaterialUI.Themes
        open Fable.MaterialUI.Props
        open Browser.Types
        open Elmish
        open Elmish.Bridge

        type ContextAction =
            | Cut
            | Copy
            | Paste
            | SelectAll
            member this.GetAction =
                match this with
                | Cut -> fun () -> cut()
                | Copy -> fun () -> copy()
                | Paste -> fun () -> paste()
                | SelectAll -> fun () -> selectAll()

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

        let init() =
            { Opened = false 
              Position =
                { X = 0
                  Y = 0 }
              MenuItems = [] }

        type Msg =
            | Open of Browser.Types.MouseEvent
            | Close
            | Action of ContextAction

        let update (msg: Msg) (model: Model) =
            match msg with
            | Open e ->
                if model.Opened then
                    model,Cmd.ofMsg Close
                else 
                    let pos = getMousePositions()
                    let element = getElementAtPos pos.X pos.Y
                    
                    let classes =
                        element 
                        |> Option.bind (fun e -> e.className |> Some) 
                        |> defaultArg <| ""
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
                        | _ ->
                            [ ]
                    if actionList.IsEmpty then model,Cmd.none
                    else
                        { model with 
                            Opened = true
                            Position =
                                { model.Position with
                                    X = pos.X
                                    Y = pos.Y }
                            MenuItems = actionList },Cmd.none
            | Close -> { model with Opened = false },Cmd.none
            | Action(f) -> 
                f.GetAction()
                model,Cmd.ofMsg Close

        let private styles (theme : ITheme) : IStyles list = []

        let private view' (classes: IClasses) model dispatch =
            let menuItems =
                model.MenuItems
                |> List.map (fun item ->
                    menuItem [
                        DOMAttr.OnClick <| fun _ -> dispatch <| Action(item.Action)
                        Style [ CSSProp.MinHeight "0em" ]
                    ] [ 
                        str item.Label
                    ])
                |> Seq.ofList

            menu [
                MenuProp.DisableAutoFocusItem true
                MaterialProp.KeepMounted true
                MaterialProp.Open model.Opened
                MaterialProp.OnClose <| fun _ -> dispatch Close
                Style [ CSSProp.TransitionDuration "5ms" ]
                PaperProps [
                    Style [ CSSProp.Width "10em" ]
                ]
                
                PopoverProp.AnchorReference AnchorReference.AnchorPosition
                PopoverProp.AnchorPosition { left = (model.Position.X); top = (model.Position.Y) }
            ] menuItems
                
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
