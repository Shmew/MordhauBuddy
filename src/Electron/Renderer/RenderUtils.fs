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
    
    let isWindows = Node.Api.``process``.platform = Node.Base.Platform.Win32
    
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