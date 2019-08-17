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

    type KeyValues =
        { Key : string
          Default : KeyValues.Values
          Value : KeyValues.Values option }

    [<RequireQualifiedAccess>]
    type File =
        | Game
        | Engine
        | GameUserSettings
        member this.Name = this.ToString() + ".ini"

    type OptionGroup =
        { Title : string
          Caption : string
          Settings : KeyValues list
          File : File
          Enabled : bool }

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
        | Parse of bool
        | Backup of bool
        | DefaultDir of string option
        | CommitChanges of bool
        | Faces of FaceResult
        | Config of ConfigResult

    [<RequireQualifiedAccess>]
    type Caller =
        | FaceTools
        | MordhauConfig

    type BridgeMsg =
        { Caller : Caller
          File : File option
          BridgeResult : BridgeResult }

    type RemoteClientMsg =
        | Resp of BridgeMsg
        | Connected
        | Disconnected

    [<AutoOpen>]
    module INITypes =
        type Selectors =
            { Selectors : string list }

        type INIFile =
            { File : File
              WorkingDir : string option }

        type FileOperation =
            | Replace of string * Selectors * INIFile
            | Delete of Selectors * INIFile
            | Exists of INIFile
            | Parse of INIFile
            | Backup of INIFile list
            | DefaultDir
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

    type RemoteServerMsg = INIOps of INIOperations * Caller

    let port = "8085" |> uint16
    let endpoint = sprintf "http://localhost:%i" port
