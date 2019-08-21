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
          Directory : string
          UpdateFun : unit -> unit
          CompleteFun : unit -> unit
          ErrorFun : unit -> unit }

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

        /// Helper module for streams
        module Streaming =
            open System.IO

            /// Download a file with continuations and progress updates
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
                gList |> List.map (fun c -> getDirect (c.DownloadUrl, None))
            get ("/repos/MordhauMappingModding/InfoFiles/contents", None)
            |> Result.map (Json.deserializeEx<GHContents list> Json.config)
            |> Result.map downloadInfoFiles

        /// Get map zip list
        let getMapFiles() =
            get ("/repos/MordhauMappingModding/MapsFiles/contents", None)
            |> ResultParser.toString
            |> Json.deserializeEx<GHContents list> Json.config

        /// Download a file to the given directory with continuations -- finish this
        let downloadFile (download : DownloadFile) =
            let dir = IO.DirectoryInfo(download.Directory)
            let updateStatus = (fun (s : string) -> async { return () }) |> Some
            async { return getStream (download.Url, None) |> ResultParser.toGeneric }
            |> Streaming.downloadFile dir download.FileName updateStatus

    let getVer (vStr : string) =
        let tryInt s =
            try
                s |> int
            with _ -> 0
        match vStr.Split('.') with
        | sList when sList.Length = 3 ->
            { MapVersion.Major = sList.[0] |> tryInt
              MapVersion.Minor = sList.[1] |> tryInt
              MapVersion.Patch = sList.[2] |> tryInt }
        | sList when sList.Length = 2 ->
            { MapVersion.Major = sList.[0] |> tryInt
              MapVersion.Minor = sList.[1] |> tryInt
              MapVersion.Patch = 0 }
        | [| s |] ->
            { MapVersion.Major = s |> tryInt
              MapVersion.Minor = 0
              MapVersion.Patch = 0 }
        | _ ->
            { MapVersion.Major = 0
              MapVersion.Minor = 0
              MapVersion.Patch = 0 }

    /// Because Json is too complex
    let getComMap (sInfoFile : string) =
        let infoArr = sInfoFile.Split([| "\r\n"; "\r"; "\n" |], StringSplitOptions.None)

        let mapEmpty s =
            if s = "" then None
            else Some(s)
        if infoArr.Length < 9 then None
        else
            System.Console.WriteLine(sInfoFile)
            { Name =
                  if infoArr.[0] = "" then None
                  else Some(infoArr.[0])
              Folder = infoArr.[1]
              Description = infoArr.[2] |> mapEmpty
              Author = infoArr.[3] |> mapEmpty
              Version = infoArr.[4] |> getVer
              ReleaseDate =
                  match DateTime.TryParse(infoArr.[5]) with
                  | (true, dt) -> dt |> Some
                  | _ -> None
              FileSize =
                  Text.RegularExpressions.Regex(@"^\d+").Match(infoArr.[6])
                  |> function
                  | s when s.Success ->
                      match Double.TryParse s.Value with
                      | (true, d) -> d * 1.0<MB> |> Some
                      | _ -> None
                  | _ -> None
              Players =
                  match infoArr.[7] with
                  | p when p.Split('-').Length = 1 ->
                      match Int32.TryParse p with
                      | (true, i) ->
                          i
                          |> SuggestedPlayers.Static
                          |> Some
                      | _ -> None
                  | pRange when pRange.Split('-').Length = 2 ->
                      let range = pRange.Split('-')
                      let min = range.[0]
                      let max = range.[1]
                      match Int32.TryParse min, Int32.TryParse max with
                      | (true, mi), (true, ma) ->
                          { PlayerRange.Min = mi
                            PlayerRange.Max = ma }
                          |> SuggestedPlayers.Range
                          |> Some
                      | _ -> None
                  | _ -> None
              Image =
                  match Uri.TryCreate(infoArr.[8], UriKind.Absolute) with
                  | (true, uri) -> uri |> Some
                  | _ -> None }
            |> Some
