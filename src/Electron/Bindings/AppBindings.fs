namespace MordhauBuddy.App

module AppBindings =
    open Fable.Core
    open Fable.Core.JsInterop
    open Fable.Import

    let version : string =
        let jObj : obj = importDefault "../../../package.json"
        jObj?version
