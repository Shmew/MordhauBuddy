namespace MordhauBuddy.Core

#nowarn "40"

module rec Helpers =
    open Fake.Core
    open Fake.IO.FileSystemOperators
    open FSharp.Data
    open FSharp.Json
    open Logary
    open Logary.Message
    open MordhauBuddy.Shared.ElectronBridge
    open Printf
    open System
    open System.Net

    /// Attempt to parse an int returning an option
    let tryParseInt mId =
        try Some (Int32.Parse mId)
        with _ -> None

    /// Functions to gather information
    module Info =
        open System.Reflection

        /// Get application version
        let version =
            Assembly.GetExecutingAssembly().GetName().Version
            |> (fun v -> sprintf "%i.%i.%i" v.Major v.Minor v.Build)

        /// Current executable directory
        let appPath = Environment.CurrentDirectory

        /// Get application name
        let appFile ver =
            if Environment.isLinux
            then sprintf "MordhauBuddy-%s.AppImage" ver
            else sprintf "MordhauBuddy.Setup.%s.exe" ver

        /// Check if Mordhau is running
        let isMordhauRunning() =
            Process.getAllByName "Mordhau" |> Seq.exists (fun p -> p.ProcessName = "Mordhau-Win64-Shipping")

        /// Try to get an env variable with increasing scope
        let tryGetEnvVar (s: string) =
            let envOpt (envVar: string) =
                if String.isNullOrEmpty envVar then None else Some(envVar)

            let procVar = Environment.GetEnvironmentVariable(s) |> envOpt
            let userVar = Environment.GetEnvironmentVariable(s, EnvironmentVariableTarget.User) |> envOpt
            let machVar = Environment.GetEnvironmentVariable(s, EnvironmentVariableTarget.Machine) |> envOpt

            match procVar, userVar, machVar with
            | Some(v), _, _
            | _, Some(v), _
            | _, _, Some(v) -> Some(v)
            | _ -> None

        [<RequireQualifiedAccess>]
        module Windows =
            open Fake.Windows

            let regBase = Registry.RegistryBaseKey.HKEYCurrentUser
            let regKey = @"Software\Microsoft\Windows\CurrentVersion\Run"
            let regSubValue = "MordhauBuddy"

            /// Get Windows executable directory
            let getWinExecDir() =
                Environment.SpecialFolder.LocalApplicationData
                |> Environment.GetFolderPath
                |> fun d -> (new IO.DirectoryInfo(d)).FullName @@ "mordhau-buddy-updater"

            /// Get Windows executable path
            let getWinExecPath() = getWinExecDir() @@ "installer.exe"

        [<RequireQualifiedAccess>]
        module Linux =
            /// Try to get home path
            let homePath = tryGetEnvVar "HOME"

            /// Get the name of the AppImage
            let appImageName = sprintf "MordhauBuddy-%s.AppImage" version

            /// Gets the home share falling back to `appPath` if necessary
            let homeShare = defaultArg (homePath |> Option.map (fun p -> p @@ ".local/share/mordhau-buddy")) appPath

            /// Name of the home json file
            let homeName = sprintf "home-%s.json" version

        let updaterPath (subDir: string) =
            if Environment.isLinux
            then Info.Linux.homeShare @@ subDir
            else Info.Windows.getWinExecDir() @@ subDir

    /// Logging helper functions and initialization
    [<AutoOpen>]
    module Logger =
        open Logary.Targets.FileSystem

        /// Determine logging directory path
        let logPath: FolderPath = Info.updaterPath "logging"

        module private Impl =
            open Logary.Targets
            open Logary.Configuration
            open Logary.Targets.File

            /// Define logging file naming scheme
            let fileConf =
                File.FileConf.create logPath (Naming("{service}-{host}-{datetime}", "log"))
                |> fun f ->
                    { f with
                          policies =
                              Rotation.Rotate
                                  ([ RotationPolicy.FileSize(MiB 5L) ], [ DeletionPolicies.maxNoOfFiles 5s ]) }

            /// Create logging targets
            let targets =
                [ File.create fileConf "test"
                  LiterateConsole.create LiterateConsole.empty "console" ]

            let service = "MordhauBuddy"
            let host = "Core"

            /// Create logary LogNManager
            let logary =
                Config.create service host
                |> Config.targets targets
                |> Config.ilogger (ILogger.Console Debug)
                |> Config.buildAndRun

            /// Create a logger
            let getLogger (sOpt: string option) =
                match sOpt with
                | Some(s) -> sprintf "%s.%s.%s" Impl.service Impl.host s
                | None -> sprintf "%s.%s" Impl.service Impl.host
                |> PointName.parse
                |> Impl.logary.getLogger

        type IEvent =
            abstract Event: value:string -> level:LogLevel -> Message

        type Logger(?logSource: string, ?event: IEvent) =
            let logger = Impl.getLogger logSource
            let ev = defaultArg (event |> Option.map (fun iEv -> iEv.Event)) eventX
            member this.Logger = logger

            /// Accepts a `string` or `StringFormat`
            ///
            /// Log a verbose message and consume the input
            member this.LogVerbose<'T>(format: StringFormat<'T, unit>): 'T = kprintf (ev >> logger.verbose) format

            /// Accepts a `string` or `StringFormat`
            ///
            /// Log an info message and consume the input
            member this.LogInfo<'T>(format: StringFormat<'T, unit>): 'T = kprintf (ev >> logger.info) format

            /// Accepts a `string` or `StringFormat`
            ///
            /// Log a debug message and consume the input
            member this.LogDebug<'T>(format: StringFormat<'T, unit>): 'T = kprintf (ev >> logger.debug) format

            /// Accepts a `string` or `StringFormat`
            ///
            /// Log a warning message and consume the input
            member this.LogWarn<'T>(format: StringFormat<'T, unit>): 'T = kprintf (ev >> logger.warn) format

            /// Accepts a `string` or `StringFormat`
            ///
            /// Log an error message and consume the input
            member this.LogError<'T>(format: StringFormat<'T, unit>): 'T = kprintf (ev >> logger.error) format

            /// Accepts a `string` or `StringFormat`
            ///
            /// Log a fatal message and consume the input
            member this.LogFatal<'T>(format: StringFormat<'T, unit>): 'T = kprintf (ev >> logger.fatal) format

    /// Generic IO tasks
    [<AutoOpen>]
    module GenericIO =
        open Fake.IO
        open Fake.IO.Globbing.Operators
        open System.IO

        let logger = Logger "Helpers.GenericIO"

        let deleteFileIgnore (filePath: string) =
            try
                async { File.Delete filePath } |> Async.RunSynchronously
            with e ->
                logger.LogError "Error deleting file: %s\n%s\n%s" filePath e.Message e.StackTrace
                ()

        let deleteDir (dirPath: string) =
            try
                async { Shell.deleteDir dirPath } |> Async.RunSynchronously
            with e ->
                logger.LogError "Error deleting directory: %s\n%s\n%s" dirPath e.Message e.StackTrace
                ()

        let cleanUpLogging (dir: string) =
            let di = DirectoryInfo.ofPath dir

            if di.Exists && di.GetFiles().Length > 5 then
                !!(di.FullName @@ "*.log")
                |> List.ofSeq
                |> List.map (fun f -> FileInfo.ofPath (di.FullName @@ f))
                |> List.sortByDescending (fun f -> f.LastWriteTimeUtc)
                |> List.chunkBySize 5
                |> List.tail
                |> List.concat
                |> List.iter (fun f -> deleteFileIgnore f.FullName)

    /// Http related helper fuctions
    module Http =
        open FSharp.Data.JsonExtensions

        /// Record for download requests
        type DownloadFile =
            { Url: string
              FileName: string
              Name: string
              CacheDirectory: IO.DirectoryInfo
              Directory: IO.DirectoryInfo
              ModInfo: ModInfoFile
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
                  Label: string option
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
                  Assets: Asset list
                  Body: string }

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
                |> Result.Error

        let httpStreamOk (resp: HttpResponseWithStream) =
            match resp.StatusCode with
            | e when e < 300 && e >= 200 -> resp.ResponseStream |> Ok
            | _ -> "Error downloading file." |> Result.Error

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
