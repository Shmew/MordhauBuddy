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
          Size : float<MB>
          UpdateFun : int -> unit
          CompleteFun : unit -> unit
          ErrorFun : string -> unit
          CancelFun : OperationCanceledException -> unit }

    module GHTypes =
        /// Record to represent GH content api response
        type GHContents =
            { Name : string
              Path : string
              Sha : string
              Size : int64
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
            open SharpCompress

            /// Download a file with continuations and progress updates
            let downloadFile (downloadFile : DownloadFile) (stream : Async<Result<Stream, string>>) =
                let errorMsg (e : exn) = sprintf "Error fetching file: %s%c%s" downloadFile.FileName '\n' (e.Message)

                let download =
                    async {
                        let mutable downloading = true
                        let mutable extracting = true
                        let! requestRes = stream

                        let position (streamDest : Stream) =
                            Math.DivRem(streamDest.Position, 1000000L)
                            |> fun (i1, i2) -> sprintf "%i.%i" i1 i2
                            |> float
                            |> (*) 1.0<MB>

                        let request =
                            match requestRes with
                            | Ok(s) -> s
                            | Error(e) -> failwith e

                        use streamDest = File.Create(downloadFile.Directory.FullName + @"\" + downloadFile.FileName)


                        let downloadUpdater (streamDest : Stream) =
                            async {
                                while downloading do
                                    downloadFile.UpdateFun(position streamDest / downloadFile.Size
                                                            |> float
                                                            |> (*) 250.
                                                            |> int)
                                    Async.Sleep 200 |> Async.RunSynchronously
                            }

                        let extractUpdater (streamDest : Stream) =
                            async {
                                while extracting do
                                    downloadFile.UpdateFun(position streamDest / downloadFile.Size
                                                            |> float
                                                            |> (*) 62.5
                                                            |> int)
                                    Async.Sleep 200 |> Async.RunSynchronously
                            }

                        async {
                            do! downloadUpdater streamDest
                            do! request.CopyToAsync(streamDest) |> Async.AwaitTask
                            downloading <- false
                            do! Async.Sleep 1000
                            do request.Dispose()
                            do streamDest.Dispose()

                            use zipStream =
                                File.OpenRead(downloadFile.Directory.FullName + @"\" + downloadFile.FileName)
                            do! extractUpdater zipStream
                            use archiveReader = SharpCompress.Readers.ReaderFactory.Open(zipStream)
                            while archiveReader.MoveToNextEntry() do
                                if archiveReader.Entry.IsDirectory then
                                    archiveReader.Entry.Key
                                    |> fun k ->
                                        Directory.CreateDirectory(downloadFile.Directory.FullName + @"\" + k)
                                    |> ignore
                                else
                                    use entryStream = archiveReader.OpenEntryStream()
                                    File.Create(downloadFile.Directory.FullName + @"\" + archiveReader.Entry.Key)
                                    |> entryStream.CopyToAsync
                                    |> Async.AwaitTask
                                    |> Async.RunSynchronously
                            extracting <- false
                        }
                        |> Async.RunSynchronously
                        |> ignore
                    }

                let onError (e : exn) = errorMsg e |> downloadFile.ErrorFun
                Async.StartWithContinuations(download, downloadFile.CompleteFun, onError, downloadFile.CancelFun)

        /// Json helpers
        module Json =
            /// Set FSharp.Json configuration
            let config =
                JsonConfig.create
                    (jsonFieldNaming = Json.lowerCamelCase, allowUntyped = true, serializeNone = SerializeNone.Omit)

    /// Functions for interacting with the Github Api
    module WebRequests =
        open Helpers
        open GHTypes

        [<NoComparison>]
        type GDKey =
            { Key : string option }

        /// Github base uri
        let private baseUri = @"https://api.github.com"

        /// Header options for requests
        [<RequireQualifiedAccess>]
        type internal ReqHeaders =
            | Github
            | GoogleDrive
            member this.Headers =
                match this with
                | Github ->
                    [ HttpRequestHeaders.ContentLanguage HttpContentTypes.Json
                      HttpRequestHeaders.Accept @"*/*"
                      HttpRequestHeaders.AcceptEncoding "UTF8"
                      HttpRequestHeaders.ContentType HttpContentTypes.Json
                      HttpRequestHeaders.ContentEncoding "UTF8"
                      HttpRequestHeaders.UserAgent "MordhauBuddy" ]
                | GoogleDrive ->
                    [ HttpRequestHeaders.Accept @"*/*"
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

        let internal get (path : string, parameters : string option, headers : ReqHeaders) =
            let req() =
                Http.Request
                    (baseUri + path + defaultArg parameters "", httpMethod = "GET", headers = headers.Headers,
                     timeout = 100000)
            makeRequest req

        let internal getDirect (fullPath : string, parameters : string option, headers : ReqHeaders) =
            let req() =
                Http.Request
                    (fullPath + defaultArg parameters "", httpMethod = "GET", headers = headers.Headers,
                     timeout = 100000)
            makeRequest req

        let internal getStream (downloadUrl : string, parameters : string option, headers : ReqHeaders) =
            let req() =
                Http.RequestStream
                    (downloadUrl + defaultArg parameters "", httpMethod = "GET", headers = headers.Headers,
                     timeout = 100000)
            makeRequestStream req

        let internal getGDStream (downloadUrl : string, parameters : string option, headers : ReqHeaders) =
            let req() =
                Http.RequestStream
                    (downloadUrl + defaultArg parameters "", httpMethod = "GET", headers = headers.Headers,
                     timeout = 100000)
            makeRequestStream req

        /// Get info file list
        let getInfoFiles() =
            let downloadInfoFiles (gList : GHContents list) =
                gList
                |> List.map (fun c -> async { return getDirect (c.DownloadUrl, None, ReqHeaders.Github) })
                |> Async.Parallel
                |> Async.RunSynchronously
                |> List.ofArray
            get ("/repos/MordhauMappingModding/InfoFiles/contents", None, ReqHeaders.Github)
            |> Result.map (Json.deserializeEx<GHContents list> Json.config)
            |> Result.map downloadInfoFiles

        /// Get map zip list
        let getMapFiles() =
            get ("/repos/MordhauMappingModding/MapsFiles/contents", None, ReqHeaders.Github)
            |> Result.map (Json.deserializeEx<GHContents list> Json.config)

        /// Download a file to the given directory with continuations
        let downloadFile (download : DownloadFile) =
            async { return getStream (download.Url, None, ReqHeaders.Github) }
            |> fun s -> Streaming.downloadFile download s

        let downloadGDFile (download : DownloadFile) =
            let key = FileOps.Maps.tryGetGDKey() |> Option.map (Json.deserializeEx<GDKey> Json.config)
            match key with
            | Some(gdKey) ->
                [ Param(("key", gdKey.Key))
                  Param(("alt", Some("media"))) ]
                |> Http.paramBuilder
                |> (fun s -> async { return getGDStream (download.Url + s, None, ReqHeaders.GoogleDrive) })
                |> fun s -> Streaming.downloadFile download s
            | _ -> download.ErrorFun "Failed to get Google Drive key"
