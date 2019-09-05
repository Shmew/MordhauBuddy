namespace MordhauBuddy.Core

open System
open System.Net
open FSharp.Data
open FSharp.Json
open MordhauBuddy.Shared.ElectronBridge

/// Module for http related tasks
module Http =
    /// Record for download requests
    type DownloadFile =
        { Url: string
          FileName: string
          MapName: string
          Directory: IO.DirectoryInfo
          Size: float<MB>
          UpdateFun: int -> unit
          CompleteFun: unit -> unit
          ErrorFun: string -> unit
          CancelFun: OperationCanceledException -> unit }

    module GHTypes =
        /// Record to represent GH content api response
        type GHContents =
            { Name: string
              Path: string
              Sha: string
              Size: int64
              Url: string
              [<JsonField("html_url")>]
              HtmlUrl: string
              [<JsonField("html_url")>]
              GitUrl: string
              [<JsonField("download_url")>]
              DownloadUrl: string
              Type: string
              [<JsonField("_links")>]
              Links: obj }

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
            let paramBuilder (paramList: ParamValues list) =
                let urlify (k: string, v: string) = (k, WebUtility.UrlEncode(v))
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
            let unwrapTextBody (body: HttpResponseBody) =
                match body with
                | Text(t) -> t
                | Binary(_) -> ""

            /// Gets the error message(s) from a `HttpResponse` if present
            let httpErrorMsg (resp: HttpResponse) =
                let getErrorMsgs (jParse: JsonValue option) =
                    try
                        jParse.Value?errors.AsArray()
                        |> Array.map (fun elem -> elem?message.AsString())
                        |> Array.reduce (fun acc elem -> acc + ('\n'.ToString()) + elem)
                    with _ -> "Unable to parse response."

                let parseResp (jParse: JsonValue option) =
                    match jParse.IsSome with
                    | true -> getErrorMsgs jParse
                    | false -> "No message given by the server."

                resp.Body
                |> unwrapTextBody
                |> JsonValue.TryParse
                |> parseResp

            /// Returns error code message
            let httpError (code: int) (errors: string) = sprintf "%i Error:%c%s" code '\n' errors

            /// Evaluates `HttpResponse` to `Ok` or `Error`
            let httpOk (resp: HttpResponse) =
                match resp.StatusCode with
                | e when e < 300 && e >= 200 ->
                    resp.Body
                    |> unwrapTextBody
                    |> Ok
                | _ ->
                    httpErrorMsg resp
                    |> httpError resp.StatusCode
                    |> Error

            let httpStreamOk (resp: HttpResponseWithStream) =
                match resp.StatusCode with
                | e when e < 300 && e >= 200 -> resp.ResponseStream |> Ok
                | _ -> "Error downloading file." |> Error

        /// Helper module for streams
        module Streaming =
            open Fake.IO.FileSystemOperators
            open System.IO
            open System.IO.Compression
            open System.Threading

            /// Download and extract a file with continuations and progress updates
            let downloadFile (downloadFile: DownloadFile) (stream: Async<Result<Stream, string>>)
                (size: Result<string, string>) (cToken: CancellationToken) =
                let errorMsg (e: exn) = sprintf "Error fetching file: %s%c%s" downloadFile.FileName '\n' (e.Message)

                let download =
                    async {
                        let mutable downloading = true
                        let! requestRes = stream
                        let path = downloadFile.Directory.FullName @@ downloadFile.FileName

                        let dlSize =
                            match size with
                            | Ok(i) ->
                                i
                                |> float
                                |> (*) 1.<B>
                                |> convertBtoMB
                            | _ -> downloadFile.Size

                        let calcPercentage (i: int64) =
                            float (i)
                            |> (*) 1.<B>
                            |> convertBtoMB
                            |> fun mb -> mb / dlSize
                            |> (*) 100.
                            |> int

                        let request =
                            match requestRes with
                            | Ok(s) -> s
                            | Error(e) -> failwith e

                        use streamDest = File.Create path

                        let downloadUpdater (streamDest: Stream) =
                            async {
                                while downloading do
                                    try
                                        downloadFile.UpdateFun(streamDest.Position |> calcPercentage)
                                        Async.Sleep 500 |> Async.RunSynchronously
                                    with _ -> downloading <- false
                            }
                        async {
                            downloadUpdater streamDest |> Async.Start
#if NETFRAMEWORK
#else
                            do! request.CopyToAsync(streamDest, cancellationToken = cToken) |> Async.AwaitTask
#endif

                            downloading <- false
                            downloadFile.UpdateFun(100)
                            do! Async.Sleep 1000
                            request.Dispose()
                            streamDest.Dispose()
                            FileOps.Maps.deleteDir (downloadFile.Directory.FullName @@ downloadFile.MapName)
                            do! Async.Sleep 1000
                            use zipStream = ZipFile.OpenRead path
                            zipStream.ExtractToDirectory(downloadFile.Directory.FullName)
                            do! Async.Sleep 1000
                            zipStream.Dispose()
                            do! FileOps.Maps.asyncDeleteZip (path)
                        }
                        |> fun a -> Async.RunSynchronously(a, cancellationToken = cToken)
                        |> ignore
                    }

                let onError (e: exn) = errorMsg e |> downloadFile.ErrorFun
                Async.StartWithContinuations(download, downloadFile.CompleteFun, onError, downloadFile.CancelFun)

        /// Json helpers
        module Json =
            /// Set FSharp.Json configuration
            [<System.Diagnostics.CodeAnalysis.SuppressMessage("*", "*")>]
            let config =
                JsonConfig.create
                    (jsonFieldNaming = Json.lowerCamelCase, allowUntyped = true, serializeNone = SerializeNone.Omit)

    /// Functions for interacting with the Github Api
    module WebRequests =
        open Helpers
        open GHTypes
        open System.Threading

        [<NoComparison>]
        type GDKey =
            { Key: string option }

        [<NoComparison>]
        type GDSize =
            { Size: string }

        /// Github base uri
        let private ghBaseUri = @"https://api.github.com"

        /// Header options for requests
        [<RequireQualifiedAccess>]
        type private ReqHeaders =
            | Github
            | Generic
            member this.Headers =
                match this with
                | Github ->
                    [ HttpRequestHeaders.ContentLanguage HttpContentTypes.Json
                      HttpRequestHeaders.Accept @"*/*"
                      HttpRequestHeaders.AcceptEncoding "UTF8"
                      HttpRequestHeaders.ContentType HttpContentTypes.Json
                      HttpRequestHeaders.ContentEncoding "UTF8"
                      HttpRequestHeaders.UserAgent "MordhauBuddy" ]
                | Generic ->
                    [ HttpRequestHeaders.Accept @"*/*"
                      HttpRequestHeaders.UserAgent "MordhauBuddy" ]

        /// Wrapper function to make http requests
        let private makeRequest (req: unit -> HttpResponse) =
            try
                req()
            with e -> sprintf "Error making HTTP request: %s" e.Message |> failwith
            |> Http.httpOk

        let private makeRequestStream (req: unit -> HttpResponseWithStream) =
            try
                req()
            with e -> sprintf "Error making HTTP request: %s" e.Message |> failwith
            |> Http.httpStreamOk

        let private get (path: string, parameters: string option, headers: ReqHeaders) =
            let req() =
                Http.Request
                    (path + defaultArg parameters "", httpMethod = "GET", headers = headers.Headers, timeout = 100000)
            makeRequest req

        let private getStringAsync (path: string, parameters: string option, headers: ReqHeaders) =
            Http.AsyncRequestString
                (path + defaultArg parameters "", httpMethod = "GET", headers = headers.Headers, timeout = 100000)

        let private getStream (downloadUrl: string, parameters: string option, headers: ReqHeaders) =
            let req() =
                Http.RequestStream
                    (downloadUrl + defaultArg parameters "", httpMethod = "GET", headers = headers.Headers,
                     timeout = 100000)
            makeRequestStream req

        let private getGDStream (downloadUrl: string, parameters: string option, headers: ReqHeaders) =
            let req() =
                Http.RequestStream
                    (downloadUrl + defaultArg parameters "", httpMethod = "GET", headers = headers.Headers,
                     timeout = 100000)
            makeRequestStream req

        /// Get info file list
        let getInfoFiles() =
            let downloadInfoFiles (gList: GHContents list) =
                gList
                |> List.map (fun c ->
                    async {
                        try
                            let! result = getStringAsync (c.DownloadUrl, None, ReqHeaders.Github)
                            return result |> Some
                        with _ -> return None
                    })
                |> Async.Parallel
                |> Async.RunSynchronously
                |> List.ofArray
            get (ghBaseUri + "/repos/MordhauMappingModding/InfoFiles/contents", None, ReqHeaders.Github)
            |> Result.map (Json.deserializeEx<GHContents list> Json.config)
            |> Result.map downloadInfoFiles

        /// Get map zip list
        let getMapFiles() =
            get (ghBaseUri + "/repos/MordhauMappingModding/MapsFiles/contents", None, ReqHeaders.Github)
            |> Result.map (Json.deserializeEx<GHContents list> Json.config)

        /// Download a file to the given directory with continuations
        let downloadFile (cToken: CancellationToken) (download: DownloadFile) =
            async { return getStream (download.Url, None, ReqHeaders.Github) }
            |> fun s -> Streaming.downloadFile download s (Error("")) cToken

        let downloadGDFile (cToken: CancellationToken) (download: DownloadFile) =
            let key = FileOps.Maps.tryGetGDKey() |> Option.map (Json.deserializeEx<GDKey> Json.config)
            match key with
            | Some(gdKey) ->
                let size =
                    [ Param(("key", gdKey.Key))
                      Param(("fields", Some("size"))) ]
                    |> Http.paramBuilder
                    |> fun s -> get (download.Url, Some(s), ReqHeaders.Generic)
                    |> Result.map ((Json.deserializeEx<GDSize> Json.config) >> (fun gdSize -> gdSize.Size))
                [ Param(("key", gdKey.Key))
                  Param(("alt", Some("media"))) ]
                |> Http.paramBuilder
                |> (fun s -> async { return getGDStream (download.Url, Some(s), ReqHeaders.Generic) })
                |> fun s -> Streaming.downloadFile download s size cToken
                getInfoFiles()
                |> function
                | Ok(sOptList) ->
                    sOptList
                    |> List.choose id
                    |> List.tryFind (fun (i: string) ->
                        i.Split('\n')
                        |> Array.tryFind (fun s -> s.Trim() = download.MapName)
                        |> Option.isSome)
                    |> function
                    | Some(s) -> FileOps.Maps.writeFile download.Directory.FullName download.MapName s
                    | _ -> ()
                | _ -> ()
            | _ -> download.ErrorFun "Failed to get Google Drive key"

        let tryGetSteamAnnRSS() =
            async {
                try
                    let! result = getStringAsync
                                      (@"https://steamcommunity.com/games/629760/rss/", None, ReqHeaders.Generic)
                    return result |> Some
                with _ -> return None
            }
            |> Async.RunSynchronously
            |> Option.map ComOperations.getAnnouncements
            |> Option.flatten
