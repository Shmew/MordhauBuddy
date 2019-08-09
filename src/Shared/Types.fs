namespace MordhauBuddy.Shared

module ElectronBridge =
    [<AutoOpen>]
    module INITypes =
        type Selectors =
            { Selectors : string list }

        type INIFile =
            { File : string
              WorkingDir : string option }

        type FileOperation =
            | Replace of string * Selectors
            | Delete of Selectors
            | Exists of INIFile
            | Parse of INIFile
            | Backup of INIFile
            | DefaultDir
            | Commit of INIFile

        type Faces =
            | Random of string list
            | Frankenstein of string list
            | Custom of string list * string
            | ProfileList

        type INIOperations =
            | Operation of FileOperation
            | Faces of Faces
            static member Endpoint = "/ws/iniops"

    type RemoteServerMsg = INIOps of INIOperations

    [<RequireQualifiedAccess>]
    type BridgeResult =
        | Replace of bool
        | Delete of bool
        | Exists of bool
        | Parse of bool
        | Backup of bool
        | DefaultDir of string option
        | Random of bool
        | Frankenstein of bool
        | Custom of bool
        | ProfileList of (string * string) list
        | CommitChanges of bool

    type RemoteClientMsg =
        | Resp of BridgeResult
        | Connected
        | Disconnected

    let port = "8085" |> uint16
    let endpoint = sprintf "http://localhost:%i" port
