namespace MordhauBuddy.Shared

module ElectronBridge =
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
            { Min: MutableValues
              Max: MutableValues }

    type KeyValues =
        { Key: string
          Default: KeyValues.Values
          Value: KeyValues.Values option
          Mutable: KeyValues.Mutable option }
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
        { Title: string
          Caption: string
          Settings: KeyValues list
          File: ConfigFile
          Enabled: bool
          Expanded: bool }

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
    type BridgeResult =
        | Replace of bool
        | Delete of bool
        | Exists of bool
        | MapDirExists of bool
        | Parse of bool
        | Backup of bool
        | DefaultDir of string option
        | DefaultMapDir of string option
        | CommitChanges of bool
        | Faces of FaceResult
        | Config of ConfigResult

    [<RequireQualifiedAccess>]
    type Caller =
        | FaceTools
        | MordhauConfig
        | Settings
        | App

    [<AutoOpen>]
    module INITypes =
        type Selectors =
            { Selectors: string list }

        type INIFile =
            { File: ConfigFile
              WorkingDir: string option }

        type FileOperation =
            | Replace of string * Selectors * INIFile
            | Delete of Selectors * INIFile
            | Exists of INIFile
            | MapDirExists of string
            | Parse of INIFile
            | Backup of INIFile list
            | DefaultDir
            | DefaultMapDir
            | Commit of INIFile list

        type Faces =
            | Random of string list
            | Frankenstein of string list
            | Custom of string list * string
            | ProfileList

        type Configs =
            | GetConfigs of OptionGroup list
            | MapConfigs of OptionGroup list

        type INIOperations =
            | Operation of FileOperation
            | Faces of Faces
            | Configs of Configs
            static member Endpoint = "/ws/iniops"

    type BridgeMsg =
        { Caller: Caller
          File: INIFile option
          BridgeResult: BridgeResult }

    type RemoteClientMsg =
        | Resp of BridgeMsg
        | Connected
        | Disconnected

    type RemoteServerMsg = INIOps of INIOperations * Caller

    let port = "8085" |> uint16
    let endpoint = sprintf "http://localhost:%i" port
