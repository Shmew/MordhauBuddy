namespace MordhauBuddy.Shared

module ElectronBridge =
    [<AutoOpen>]
    module UnitsOfMeasure =
        [<Measure>]
        type B

        [<Measure>]
        type KB

        [<Measure>]
        type MB

        [<Measure>]
        type GB

        let bPerKb : float<B / KB> = 1000.<B/KB>
        let bPerMb : float<B / MB> = 1000000.<B/MB>
        let kbPerMb : float<KB / MB> = 1000.<KB/MB>
        let mbPerKb : float<MB / KB> = 0.001<MB/KB>
        let gbPerMb : float<GB / MB> = 0.001<GB/MB>
        let convertBtoKB (x : float<B>) = x / bPerKb
        let convertBtoMB (x : float<B>) = x / bPerMb
        let convertKBtoMB (x : float<KB>) = x / kbPerMb
        let convertMBtoKB (x : float<MB>) = x / mbPerKb
        let convertGBtoMB (x : float<GB>) = x / gbPerMb

    [<RequireQualifiedAccess>]
    module KeyValues =
        [<RequireQualifiedAccess>]
        type Values =
            | String of string
            | Bool of bool
            | Int of int
            | Float of float

            override this.ToString() =
                match this with
                | String(s) -> s |> string
                | Bool(b) -> b |> string
                | Int(i) -> i |> string
                | Float(f) -> f |> string

            member this.TryFloat() =
                match this with
                | Int(i) ->
                    i
                    |> float
                    |> Some
                | Float(f) -> f |> Some
                | _ -> None

        [<RequireQualifiedAccess>]
        type MutableValues =
            | MutInt of int
            | MutFloat of float
            member this.ToFloat() =
                match this with
                | MutInt(i) -> i |> float
                | MutFloat(f) -> f

        [<RequireQualifiedAccess>]
        type Mutable =
            { Min : MutableValues
              Max : MutableValues
              Step : float }

    type KeyValues =
        { Key : string
          Default : KeyValues.Values
          Value : KeyValues.Values option
          Mutable : KeyValues.Mutable option }
        member this.GetDefault() =
            match this.Value, this.Default.TryFloat() with
            | Some(v), _ when v.TryFloat().IsSome -> v.TryFloat().Value
            | _, Some(d) -> d
            | _ -> 0.

    [<RequireQualifiedAccess>]
    type ConfigFile =
        | Game
        | Engine
        | GameUserSettings
        member this.Name = this.ToString() + ".ini"

    type OptionGroup =
        { Title : string
          Caption : string
          Settings : KeyValues list
          File : ConfigFile
          Enabled : bool
          Expanded : bool }

    [<RequireQualifiedAccess>]
    type FaceResult =
        | Random of bool
        | Frankenstein of bool
        | Custom of bool
        | ProfileList of (string * string) list

    [<RequireQualifiedAccess>]
    type ConfigResult =
        | GetConfigs of OptionGroup list
        | MapConfigs of bool

    [<RequireQualifiedAccess>]
    type INIOperationResult =
        | DefaultDir of string option
        | Replace of bool
        | Delete of bool
        | Exists of bool
        | Parse of bool
        | Backup of bool
        | CommitChanges of bool

    [<RequireQualifiedAccess>]
    type MapOperationResult =
        | DefaultDir of string option
        | DirExists of bool
        | Delete of string * Result<bool, string>

    [<RequireQualifiedAccess>]
    type MapResult =
        | AvailableMaps of string list
        | InstalledMaps of string list
        | InstallMap of string * Result<bool, string>
        | InstallMapCancelled of string * bool
        | InstallMapProgress of string * int
        | InstallMapComplete of string
        | InstallMapError of string * string

    [<RequireQualifiedAccess>]
    type BridgeResult =
        | INIOperation of INIOperationResult
        | MapOperation of MapOperationResult
        | Faces of FaceResult
        | Config of ConfigResult
        | Maps of MapResult

    [<RequireQualifiedAccess>]
    type Caller =
        | MapInstaller
        | FaceTools
        | MordhauConfig
        | Settings
        | App

    type Selectors =
        { Selectors : string list }

    type INIFile =
        { File : ConfigFile
          WorkingDir : string option }

    [<RequireQualifiedAccess>]
    type INIFileOperation =
        | DefaultDir
        | Replace of string * Selectors * INIFile
        | Delete of Selectors * INIFile
        | Exists of INIFile
        | Parse of INIFile
        | Backup of INIFile list
        | Commit of INIFile list

    [<RequireQualifiedAccess>]
    type MapFileOperation =
        | DefaultDir
        | DirExists of string
        | Delete of string * string

    type Faces =
        | Random of string list
        | Frankenstein of string list
        | Custom of string list * string
        | ProfileList

    type Configs =
        | GetConfigs of OptionGroup list
        | MapConfigs of OptionGroup list

    [<RequireQualifiedAccess>]
    type GoogleDrive =
        { ID : string
          Size : float<MB> }

    [<RequireQualifiedAccess>]
    type MapTarget =
        { Folder : string
          Directory : string
          GDrive : GoogleDrive option }

    type Maps =
        | GetAvailableMaps
        | GetInstalledMaps of string
        | InstallMap of MapTarget
        | ConfirmInstalled of string
        | CancelMap of string

    type BridgeOperations =
        | INIOperation of INIFileOperation
        | MapOperation of MapFileOperation
        | Faces of Faces
        | Configs of Configs
        | Maps of Maps
        static member Endpoint = "/ws"

    type BridgeMsg =
        { Caller : Caller
          File : INIFile option
          BridgeResult : BridgeResult }

    type RemoteClientMsg =
        | Resp of BridgeMsg
        | Connected
        | Disconnected

    type RemoteServerMsg = BridgeOps of BridgeOperations * Caller

    let port = "8085" |> uint16
    let endpoint = sprintf "http://localhost:%i" port
