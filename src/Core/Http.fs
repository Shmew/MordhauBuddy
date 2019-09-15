namespace MordhauBuddy.Core

open MordhauBuddy.Core
open Helpers
open Helpers.Http
open System.Net
open FSharp.Data
open FSharp.Json
open MordhauBuddy.Shared.ElectronBridge

/// Module for http related tasks
module Http =
    /// Helper module for streams
    module Streaming =
        open Fake.IO.FileSystemOperators
        open System.IO
        open System.IO.Compression
        open System.Threading

        let logger = Logger "Http.Streaming"

        /// Download and extract a zip file with continuations and progress updates
        let downloadZip (downloadFile: DownloadFile) (stream: Async<Result<Stream, string>>)
            (size: Result<string, string>) (cToken: CancellationToken) =
            let errorMsg (e: exn) = sprintf "Error fetching file: %s%c%s" downloadFile.FileName '\n' e.Message
            let path = downloadFile.Directory.FullName @@ downloadFile.FileName

            let download =
                async {
                    let mutable downloading = true
                    let! requestRes = stream

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
                        deleteDir (downloadFile.Directory.FullName @@ downloadFile.Name)
                        do! Async.Sleep 1000
                        use zipStream = ZipFile.OpenRead path
                        zipStream.ExtractToDirectory(downloadFile.Directory.FullName)
                        do! Async.Sleep 1000
                        zipStream.Dispose()
                        deleteFileIgnore(path)
                    }
                    |> fun a -> Async.RunSynchronously(a, cancellationToken = cToken)
                    |> ignore
                }

            let onError (e: exn) =
                logger.LogError "Error fetching file:\n\t%O\n%O" downloadFile e
                deleteFileIgnore(path)
                errorMsg e |> downloadFile.ErrorFun

            let onCancel (cancelFun: System.OperationCanceledException -> unit)
                (cExn: System.OperationCanceledException) =
                deleteFileIgnore(path)
                cancelFun cExn

            Async.StartWithContinuations
                (download, downloadFile.CompleteFun, onError, (onCancel downloadFile.CancelFun))

    /// Functions for interacting with the Github Api
    module WebRequests =
        open System.Threading

        let logger = Logger "Http.WebRequests"

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
            with e -> 
                logger.LogError "Error making HTTP request:\n%O" e
                raise <| System.Exception()
            |> Http.httpOk

        let private makeRequestStream (req: unit -> HttpResponseWithStream) =
            try
                req()
            with e -> 
                logger.LogError "Error making HTTP request:\n%O" e
                raise <| System.Exception()
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

        /// Download a zip file to the given directory with continuations
        let downloadZipFile (cToken: CancellationToken) (download: DownloadFile) =
            async { return getStream (download.Url, None, ReqHeaders.Github) }
            |> fun s -> Streaming.downloadZip download s (Error("")) cToken

        /// Pulls Mordhau rss feed
        let tryGetSteamAnnRSS() =
            async {
                try
                    let! result = getStringAsync
                                      (@"https://steamcommunity.com/games/629760/rss/", None, ReqHeaders.Generic)
                    return result |> Some
                with e -> 
                    logger.LogError "Failed to fetch Mordhau Steam rss feed:\n%O" e
                    return None
            }
            |> Async.RunSynchronously
            |> Option.map ComOperations.getAnnouncements
            |> Option.flatten

        /// Get MordhauBuddy releases
        let getReleases() =
            get (ghBaseUri + "/repos/Shmew/MordhauBuddy/releases", None, ReqHeaders.Github)
            |> Result.map (Json.deserializeEx<Github.Release list> Json.config)
