namespace MordhauBuddy.Core

open System
open System.Net
open System.Net.Security
open System.Text.RegularExpressions
open FSharp.Data
open FSharp.Json

/// Module for map related tasks
module Maps =
    [<AutoOpen>]
    module GHResponses =
        type Contents =
            { Name : string
              Path : string
              Sha : string
              Size : int
              Url : string
              [<JsonField("html_url")>]
              HtmlUrl : string
              [<JsonField("html_url")>]
              GitUrl : string
              [<JsonField("download_url")>]
              DownloadUrl : string
              Type : string
              [<JsonField("_links")>]
              Links : obj }

    [<AutoOpen>]
    module Helpers =
        /// Single case DU for creating GET queries
        type ParamValues =
            | Param of string * string option
            | Flag of string * bool

            /// Returns `Some` key value pair if it exists `None` otherwise
            member this.TryGetParamValue() =
                match this with
                | Param(a, b) when b.IsSome -> Some((a, b.Value))
                | _ -> None

            /// Returns boolean response as to parameter value validity
            member this.IsValid =
                match this with
                | Param(_, b) when b.IsSome -> true
                | Flag(_, b) when b -> true
                | _ -> false

        /// Functions to unwrap `Result<'t,string>` into various outputs
        [<RequireQualifiedAccess>]
        module ResultParser =
            /// Applies `Result<'t,string>` to given function
            let rParser (resp : Result<'t, string>) f =
                match resp with
                | Ok(r) -> r |> f
                | Error(e) -> failwith e

            /// Gets `JsonValue`
            let toJson (resp : Result<string, string>) = rParser resp JsonValue.Parse

            /// Gets `string`
            let toString (resp : Result<string, string>) = rParser resp string

            /// Gets `'t` of `Result<'t,string>` by applying identity function
            let toGeneric (resp : Result<'t, string>) = rParser resp id

        /// Http related helper fuctions
        module Http =
            open FSharp.Data.JsonExtensions

            /// Maps and encodes input list to GET query format
            let paramBuilder (paramList : ParamValues list) =
                let urlify (k : string, v : string) = (k, WebUtility.UrlEncode(v))
                paramList
                |> List.filter (fun e -> e.IsValid)
                |> List.map (fun elem ->
                       match elem with
                       | Param(a, b) -> urlify (a, b.Value) |> (fun (k, v) -> sprintf "%s=%s" k v)
                       | Flag(a, _) -> a)
                |> (fun p ->
                match p with
                | _ when p |> List.isEmpty -> ""
                | pList ->
                    pList
                    |> List.reduce (fun acc elem -> acc + "&" + elem)
                    |> (+) "?")

            /// Converts a text `HttpResponseBody` to string
            let unwrapTextBody (body : HttpResponseBody) =
                match body with
                | Text(t) -> t
                | Binary(_) -> ""

            /// Gets the error message(s) from a `HttpResponse` if present
            let httpErrorMsg (resp : HttpResponse) =
                let getErrorMsgs (jParse : JsonValue option) =
                    try
                        jParse.Value?errors.AsArray()
                        |> Array.map (fun elem -> elem?message.AsString())
                        |> Array.reduce (fun acc elem -> acc + ('\n'.ToString()) + elem)
                    with _ -> "Unable to parse response."

                let parseResp (jParse : JsonValue option) =
                    match jParse.IsSome with
                    | true -> getErrorMsgs jParse
                    | false -> "No message given by the server."

                resp.Body
                |> unwrapTextBody
                |> JsonValue.TryParse
                |> parseResp

            /// Returns error code message
            let httpError (code : int) (errors : string) = sprintf "%i Error:%c%s" code '\n' errors

            /// Evaluates `HttpResponse` to `Ok` or `Error`
            let httpOk (resp : HttpResponse) =
                match resp.StatusCode with
                | e when e < 300 && e >= 200 ->
                    resp.Body
                    |> unwrapTextBody
                    |> Ok
                | _ ->
                    httpErrorMsg resp
                    |> httpError resp.StatusCode
                    |> Error

            let httpStreamOk (resp : HttpResponseWithStream) =
                match resp.StatusCode with
                | e when e < 300 && e >= 200 -> resp.ResponseStream |> Ok
                | _ -> "Error downloading file." |> Error

        module Streaming =
            open System.IO

            let downloadFile (dest : DirectoryInfo) (fileName : string) (updateStatus : (string -> Async<unit>) option)
                (stream : Async<Stream>) =
                let errorMsg (e : exn) = sprintf "Error fetching file: %s%c%s" fileName '\n' (e.Message)

                let setStatus (newStatus : string) =
                    match updateStatus with
                    | Some(statusFunc) -> statusFunc newStatus |> Async.Ignore
                    | None -> async { return () }

                let download =
                    async {
                        do! setStatus "S"
                        let! request = stream
                        use streamDest = new FileStream((dest.FullName + @"\" + fileName), FileMode.Create)
                        do! request.CopyToAsync(streamDest) |> Async.AwaitTask
                    }

                let onCompletion() = async { do! setStatus "C" } |> Async.RunSynchronously

                let onError (e : exn) =
                    let err = errorMsg e
                    try
                        async { do! setStatus "N" } |> Async.RunSynchronously
                    with e2 -> sprintf "%s%c%s" e2.Message '\n' err |> failwith
                    failwith err
                Async.StartWithContinuations(download, onCompletion, onError, (fun _ -> ()))

        module Json =
            let config =
                JsonConfig.create
                    (jsonFieldNaming = Json.lowerCamelCase, allowUntyped = true, serializeNone = SerializeNone.Omit)

    /// Github Rest Api wrapper
    type GHApi() =
        let baseUri = @"https://api.github.com"

        /// Constructs request headers
        member internal this.ReqHeaders(?content : string) =
            [ HttpRequestHeaders.ContentLanguage HttpContentTypes.Json
              HttpRequestHeaders.Accept @"*/*"
              HttpRequestHeaders.AcceptEncoding "UTF8"
              HttpRequestHeaders.ContentType(defaultArg content HttpContentTypes.Json)
              HttpRequestHeaders.ContentEncoding "UTF8"
              HttpRequestHeaders.UserAgent "MordhauBuddy" ]

        /// Wrapper function to make http requests
        member internal this.MakeRequest(req : unit -> HttpResponse) =
            //ServicePointManager.ServerCertificateValidationCallback <- new RemoteCertificateValidationCallback(fun _ _ _ _ ->
            //true)
            try
                req()
            with e -> sprintf "Error making HTTP request via %s%c%s" baseUri '\n' e.Message |> failwith
            |> Http.httpOk

        member internal this.MakeRequestStream(req : unit -> HttpResponseWithStream) =
            //ServicePointManager.ServerCertificateValidationCallback <- new RemoteCertificateValidationCallback(fun _ _ _ _ ->
            //true)
            try
                req()
            with e -> sprintf "Error making HTTP request via %s%c%s" baseUri '\n' e.Message |> failwith
            |> Http.httpStreamOk

        member internal this.Get(path : string, ?parameters : string, ?errors : bool, ?timeout : int) =
            let req() =
                Http.Request
                    (baseUri + path + defaultArg parameters "", httpMethod = "GET", headers = this.ReqHeaders(),
                     ?silentHttpErrors = errors, timeout = defaultArg timeout 100000)
            this.MakeRequest req

        member internal this.GetStream(downloadUrl : string, ?errors : bool, ?timeout : int) =
            let req() =
                Http.RequestStream
                    (downloadUrl, httpMethod = "GET", headers = this.ReqHeaders(), ?silentHttpErrors = errors,
                     timeout = defaultArg timeout 100000)
            this.MakeRequestStream req

        /// Get info file list
        member this.GetInfoFiles() =
            this.Get("/repos/MordhauMappingModding/InfoFiles/contents")
            |> ResultParser.toString
            |> Json.deserializeEx<Contents list> Json.config

        /// Get map zip list
        member this.GetMapFiles() =
            this.Get("/repos/MordhauMappingModding/MapsFiles/contents")
            |> ResultParser.toString
            |> Json.deserializeEx<Contents list> Json.config

        member this.DownloadFile(downloadUrl : string, fileName : string, dir : string, updater : unit -> unit, onComplete : unit -> unit, onError : unit -> unit) =
            let dir = IO.DirectoryInfo(dir)
            let updateStatus = (fun (s : string) -> async { return () }) |> Some
            async { return this.GetStream(downloadUrl) |> ResultParser.toGeneric }
            |> Streaming.downloadFile dir fileName updateStatus
