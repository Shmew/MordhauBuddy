namespace MordhauBuddy.App

module BridgeUtils =
    open MordhauBuddy.Shared.ElectronBridge
    
    [<RequireQualifiedAccess>]
    module INI =
        [<RequireQualifiedAccess>]
        module Ops =
            let private wrapOps iCmd = INIOps(Operation(iCmd))
            let replace oldI newI sels = Replace(oldI,newI,sels) |> wrapOps
            let delete oldI sels = Delete(oldI,sels) |> wrapOps
            let exists iFile = Exists(iFile) |> wrapOps
            let parse iFile = Parse(iFile) |> wrapOps
            let backup iFile = Backup(iFile) |> wrapOps
            let getGameProfiles = DefaultDir |> wrapOps

        [<RequireQualifiedAccess>]
        module Faces =
            let private wrapFace fCmd = INIOps(Faces(fCmd))
            let setRandom profile iVal = Random(profile,iVal) |> wrapFace
            let setFrankenstein profile iVal = Frankenstein(profile,iVal) |> wrapFace
            let setCustom profile iVal fVal = Custom(profile,iVal,fVal) |> wrapFace
            let getProfileList iVal = ProfileList(iVal) |> wrapFace

module RenderUtils =
    open Electron
    open Fable.Core
    open Fable.Core.JsInterop
    open Fable.Import
    
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
