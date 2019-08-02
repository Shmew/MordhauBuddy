namespace MordhauBuddy.App

module Utils =
    open Fable.Core
    open Fable.Core.JsInterop
    open Fable.Import
    open Electron
    open Node.Api

    let getRemoteWin() = renderer.remote.getCurrentWindow()

    [<Emit("__static + \"/\" + $0")>]
    let private stat' (s : string) : string = jsNative

    /// Prefixes the string with the static asset root path.
    let stat (s : string) =
#if DEBUG
        s
#else
        stat' s
#endif


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

    module String =
        open System.Text.RegularExpressions

        let ensureEndsWith (suffix : string) (str : string) =
            if str.EndsWith suffix then str
            else str + suffix

        let duToTitle (s : string) =
            MatchEvaluator(fun m -> " " + m.Value)
            |> (fun m -> Regex.Replace(s.Substring(1), "[A-Z]", m))
            |> (+) (s.Substring(0, 1))

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

            /// The saved x coordinate of the loaded state. None if the state has not
            /// been saved yet.
            abstract x : int option

            /// The saved y coordinate of the loaded state. None if the state has not
            /// been saved yet.
            abstract y : int

            /// The saved width of loaded state. `defaultWidth` if the state has not
            /// been saved yet.
            abstract width : int

            /// The saved heigth of loaded state. `defaultHeight` if the state has not
            /// been saved yet.
            abstract height : int

            /// `true` if the window state was saved while the window was maximized.
            /// None if the state has not been saved yet.
            abstract isMaximized : bool option

            /// `true` if the window state was saved while the window was in full screen
            /// mode. None if the state has not been saved yet.
            abstract isFullScreen : bool option

            /// Register listeners on the given `BrowserWindow` for events that are
            /// related to size or position changes (`resize`, `move`). It will also
            /// restore the window's maximized or full screen state. When the window is
            /// closed we automatically remove the listeners and save the state.
            abstract manage : window:BrowserWindow -> unit

            /// Removes all listeners of the managed `BrowserWindow` in case it does not
            /// need to be managed anymore.
            abstract unmanage : unit -> unit

            /// Saves the current state of the given `BrowserWindow`. This exists mostly
            /// for legacy purposes, and in most cases it's better to just use `manage`.
            abstract saveState : window:BrowserWindow -> unit

        [<AllowNullLiteral>]
        type Options =

            /// The width that should be returned if no file exists yet.
            /// Defaults to `800`.
            abstract defaultWidth : int with get, set

            /// The height that should be returned if no file exists yet.
            /// Defaults to `600`.
            abstract defaultHeight : int with get, set

            /// The path where the state file should be written to.
            /// Defaults to `app.getPath('userData')`.
            abstract path : string with get, set

            /// The name of file. Defaults to `window-state.json`.
            abstract file : string with get, set

            /// Should we automatically maximize the window, if it was last closed
            /// maximized. Defaults to `true`.
            abstract maximize : bool with get, set

            /// Should we automatically restore the window to full screen, if it was
            /// last closed full screen. Defaults to `true`.
            abstract fullScreen : bool with get, set

        let getState : Options -> State = importDefault "electron-window-state"

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
