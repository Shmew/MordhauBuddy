namespace MordhauBuddy.Core

module Helpers =
    open System
    open System.Net
    open FSharp.Data
    open FSharp.Json
    open MordhauBuddy.Shared.ElectronBridge
    
    module Info =
        open Fake.Core
        open System.Reflection

        /// Get application version
        let version =
            Assembly.GetExecutingAssembly().GetName().Version |> (fun v -> sprintf "%i.%i.%i" v.Major v.Minor v.Build)

        let appFile ver =
            if Environment.isLinux then
                sprintf "MordhauBuddy-%s.AppImage" ver
            else sprintf "MordhauBuddy.Setup.%s.exe" ver

    /// Http related helper fuctions
    module Http =
        open FSharp.Data.JsonExtensions

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

        /// Collection of Github schema types
        [<RequireQualifiedAccess>]
        module Github =
            /// Record to represent GH content api response
            type Contents =
                { Name: string
                  Path: string
                  Sha: string
                  Size: int64
                  Url: string
                  [<JsonField("html_url")>]
                  HtmlUrl: string
                  [<JsonField("git_url")>]
                  GitUrl: string
                  [<JsonField("download_url")>]
                  DownloadUrl: string
                  Type: string
                  [<JsonField("_links")>]
                  Links: obj }

            type Asset = 
                { Url: string
                  [<JsonField("browser_download_url")>]
                  BrowserDownloadUrl: string
                  Id: int
                  [<JsonField("node_id")>]
                  NodeId: string
                  Name: string
                  Label: string
                  State: string
                  [<JsonField("content_type")>]
                  ContentType: string
                  Size: int
                  [<JsonField("download_count")>]
                  DownloadCount: int
                  [<JsonField("created_at")>]
                  CreatedAt: string
                  [<JsonField("updated_at")>]
                  UpdatedAt: string
                  Uploader: obj }

            type Release =
                { Url: string
                  [<JsonField("html_url")>]
                  HtmlUrl: string
                  [<JsonField("assets_url")>]
                  AssetsUrl: string
                  [<JsonField("upload_url")>]
                  UploadUrl: string
                  [<JsonField("tarball_url")>]
                  TarBallUrl: string
                  [<JsonField("zipball_url")>]
                  ZipBallUrl: string
                  Id: int
                  [<JsonField("node_id")>]
                  NodeId: string
                  [<JsonField("tag_name")>]
                  TagName: string
                  [<JsonField("target_commitish")>]
                  TargetCommitish: string
                  Draft: bool
                  Prerelease: bool
                  [<JsonField("created_at")>]
                  CreatedAt: string
                  [<JsonField("published_at")>]
                  PublishedAt: string
                  Author: obj
                  Assets: Asset list }

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

    /// Json helpers
    module Json =

        /// Represents home file for Linux
        type HomeFile =
            { Path: string }

        /// Set FSharp.Json configuration
        [<System.Diagnostics.CodeAnalysis.SuppressMessage("*", "*")>]
        let config =
            JsonConfig.create
                (jsonFieldNaming = Json.lowerCamelCase, allowUntyped = true, serializeNone = SerializeNone.Omit)
