namespace MordhauBuddy.Core

open System
open System.Net
open FSharp.Data
open FSharp.Json
open MordhauBuddy.Shared.ElectronBridge

/// Module for map related tasks
module Maps =
    /// Record for download requests
    type DownloadFile =
        { Url : string
          FileName : string
          Directory : IO.DirectoryInfo
          UpdateFun : float -> unit
          CompleteFun : bool -> unit
          ErrorFun : string -> unit
          CancelFun : OperationCanceledException -> unit }

    module GHTypes =
        /// Record to represent GH content api response
        type GHContents =
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

        /// Helper module for streams
        module Streaming =
            open System.IO

            /// Download a file with continuations and progress updates
            let downloadFile (downloadFile : DownloadFile) (stream : Async<Result<Stream,string>>) =
                let errorMsg (e : exn) = sprintf "Error fetching file: %s%c%s" downloadFile.FileName '\n' (e.Message)

                let download =
                    async {
                        let! requestRes = stream
                        let request =
                            match requestRes with
                            | Ok(s) -> s
                            | Error(e) -> failwith e
                        let position () = request.Position / request.Length / 1000000L
                        use streamDest = new FileStream((downloadFile.Directory.FullName + @"\" + downloadFile.FileName), FileMode.Create)
                        let updater =
                            async {
                                while request.Position <> request.Length do
                                    downloadFile.UpdateFun (position() |> float)
                            }
                        let copyStream = request.CopyToAsync(streamDest) |> Async.AwaitTask
                        [ updater; copyStream ] |> Async.Parallel
                        |> Async.RunSynchronously
                        |> ignore
                    }

                let onCompletion() = true |> downloadFile.CompleteFun

                let onError (e : exn) =
                    errorMsg e |> downloadFile.ErrorFun

                Async.StartWithContinuations(download, onCompletion, onError, downloadFile.CancelFun)
                
        /// Json helpers
        module Json =
            /// Set FSharp.Json configuration
            let config =
                JsonConfig.create
                    (jsonFieldNaming = Json.lowerCamelCase, allowUntyped = true, serializeNone = SerializeNone.Omit)

    /// Functions for interacting with the Github Api
    module GHApi =
        open Helpers
        open GHTypes

        /// Github base uri
        let baseUri = @"https://api.github.com"

        /// Construct request headers
        let reqHeaders =
            [ HttpRequestHeaders.ContentLanguage HttpContentTypes.Json
              HttpRequestHeaders.Accept @"*/*"
              HttpRequestHeaders.AcceptEncoding "UTF8"
              HttpRequestHeaders.ContentType HttpContentTypes.Json
              HttpRequestHeaders.ContentEncoding "UTF8"
              HttpRequestHeaders.UserAgent "MordhauBuddy" ]

        /// Wrapper function to make http requests
        let internal makeRequest (req : unit -> HttpResponse) =
            try
                req()
            with e -> sprintf "Error making HTTP request via %s%c%s" baseUri '\n' e.Message |> failwith
            |> Http.httpOk

        let internal makeRequestStream (req : unit -> HttpResponseWithStream) =
            try
                req()
            with e -> sprintf "Error making HTTP request via %s%c%s" baseUri '\n' e.Message |> failwith
            |> Http.httpStreamOk

        let internal get (path : string, parameters : string option) =
            let req() =
                Http.Request
                    (baseUri + path + defaultArg parameters "", httpMethod = "GET", headers = reqHeaders,
                     timeout = 100000)
            makeRequest req

        let internal getDirect (fullPath : string, parameters : string option) =
            let req() =
                Http.Request
                    (fullPath + defaultArg parameters "", httpMethod = "GET", headers = reqHeaders, timeout = 100000)
            makeRequest req

        let internal getStream (downloadUrl : string, parameters : string option) =
            let req() =
                Http.RequestStream
                    (downloadUrl + defaultArg parameters "", httpMethod = "GET", headers = reqHeaders, timeout = 100000)
            makeRequestStream req

        /// Get info file list
        let getInfoFiles() =
            let downloadInfoFiles (gList : GHContents list) =
                gList
                |> List.map (fun c -> async { return getDirect (c.DownloadUrl, None) })
                |> Async.Parallel
                |> Async.RunSynchronously
                |> List.ofArray
            get ("/repos/MordhauMappingModding/InfoFiles/contents", None)
            |> Result.map (Json.deserializeEx<GHContents list> Json.config)
            |> Result.map downloadInfoFiles

        /// Get map zip list
        let getMapFiles() =
            get ("/repos/MordhauMappingModding/MapsFiles/contents", None)
            |> Result.map (Json.deserializeEx<GHContents list> Json.config)

        /// Download a file to the given directory with continuations -- finish this
        let downloadFile (download : DownloadFile) =
            async { return getStream (download.Url, None) }
            |> fun s -> Streaming.downloadFile download s
            
