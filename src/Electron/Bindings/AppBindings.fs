namespace MordhauBuddy.App

module AppBindings =
    open Electron
    open Fable.Core
    open Fable.Core.JsInterop
    open Fable.Import

    module Info =
        let private pkgJson : obj = importDefault "../../../package.json"

        let private normalizeKebabCase (s : string) =
            s.Split('-')
            |> Array.map (fun (s : string) ->
                   s.Substring(0, 1)
                   |> (fun c -> c.ToUpper())
                   |> (fun c -> c + s.Substring(1, s.Length)))
            |> Array.reduce (fun acc elem -> acc + " " + elem)

        let version : string = pkgJson?version
        let name : string = pkgJson?name |> normalizeKebabCase
        let homepage : string = pkgJson?homepage
        let description : string = pkgJson?description
        let issues : string = pkgJson?bugs?url
        let author : string = pkgJson?author
        let license : string = pkgJson?license

    module WindowState =
        type State =
            abstract x : int
            abstract y : int
            abstract width : int
            abstract height : int
            abstract isMaximized : bool
            abstract isFullScreen : bool
            abstract manage : BrowserWindow -> unit
            abstract unmanage : unit -> unit
            abstract saveState : BrowserWindow -> unit

        [<AllowNullLiteral>]
        type Options =

            /// The height that should be returned if no file exists yet. Defaults to `600`.
            abstract defaultHeight : int option with get, set

            /// The width that should be returned if no file exists yet. Defaults to `800`.
            abstract defaultWidth : int option with get, set

            abstract fullScreen : bool option with get, set

            /// The path where the state file should be written to. Defaults to `app.getPath('userData')`.
            abstract path : string option with get, set

            /// The name of file. Defaults to `window-state.json`.
            abstract file : string option with get, set

            /// Should we automatically maximize the window, if it was last closed maximized. Defaults to `true`.
            abstract maximize : bool option with get, set

        let getState : Options -> State = importDefault "electron-window-state"

    module AppStore =
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

        type StoreNum =
            { Type : string
              Maximum : int
              Minimum : int
              Default : int }

        type StoreString =
            { Type : string
              Format : string }

        type Schema =
            { Test : StoreNum }

        let getStore : StoreStatic = importDefault "electron-store"
