namespace MordhauBuddy.Core

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

        /// Download and extract a file with continuations and progress updates
        let downloadMap (downloadFile: DownloadFile) (stream: Async<Result<Stream, string>>)
            (size: Result<string, string>) (cToken: CancellationToken) =
            let errorMsg (e: exn) = sprintf "Error fetching file: %s%c%s" downloadFile.FileName '\n' (e.Message)
            let path = downloadFile.Directory.FullName @@ downloadFile.FileName

            let deleteMapIgnore() =
                try
                    FileOps.Maps.asyncDeleteZip path |> Async.Start
                with _ -> ()

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
                        FileOps.Maps.deleteDir (downloadFile.Directory.FullName @@ downloadFile.MapName)
                        do! Async.Sleep 1000
                        use zipStream = ZipFile.OpenRead path
                        zipStream.ExtractToDirectory(downloadFile.Directory.FullName)
                        do! Async.Sleep 1000
                        zipStream.Dispose()
                        deleteMapIgnore()
                    }
                    |> fun a -> Async.RunSynchronously(a, cancellationToken = cToken)
                    |> ignore
                }

            let onError (e: exn) =
                deleteMapIgnore()
                errorMsg e |> downloadFile.ErrorFun

            let onCancel (cancelFun: System.OperationCanceledException -> unit)
                (cExn: System.OperationCanceledException) =
                deleteMapIgnore()
                cancelFun cExn

            Async.StartWithContinuations
                (download, downloadFile.CompleteFun, onError, (onCancel downloadFile.CancelFun))

    /// Functions for interacting with the Github Api
    module WebRequests =
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
            let downloadInfoFiles (gList: Github.Contents list) =
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
            |> Result.map (Json.deserializeEx<Github.Contents list> Json.config)
            |> Result.map downloadInfoFiles

        /// Get map zip list
        let getMapFiles() =
            get (ghBaseUri + "/repos/MordhauMappingModding/MapsFiles/contents", None, ReqHeaders.Github)
            |> Result.map (Json.deserializeEx<Github.Contents list> Json.config)

        /// Download a file to the given directory with continuations
        let downloadFile (cToken: CancellationToken) (download: DownloadFile) =
            async { return getStream (download.Url, None, ReqHeaders.Github) }
            |> fun s -> Streaming.downloadMap download s (Error("")) cToken

        /// Downloads a file from Google Drive
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
                |> fun s -> Streaming.downloadMap download s size cToken
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

        /// Pulls Mordhau rss feed
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

        /// Get MordhauBuddy releases
        let getReleases() =
            get (ghBaseUri + "/repos/Shmew/MordhauBuddy/releases", None, ReqHeaders.Github)
            |> Result.map (Json.deserializeEx<Github.Release list> Json.config)
