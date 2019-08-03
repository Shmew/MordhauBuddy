namespace MordhauBuddy.Shared

module ElectronBridge =
    [<AutoOpen>]
    module INITypes =
        type Selectors =
            { Selectors : string list }

        type INIFile =
            { File : string
              WorkingDir : string option }

        type FaceValues =
            { Translate : int list
              Rotate : int list
              Scale : int list }

        type FileOperation =
            | Replace of string * Selectors
            | Delete of Selectors
            | Exists of INIFile
            | Parse of INIFile
            | Backup of INIFile
            | DefaultDir

        type Faces =
            | Random of string
            | Frankenstein of string
            | Custom of string * FaceValues
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
        | ProfileList of string list

    type RemoteClientMsg = Resp of BridgeResult

    let port = "8085" |> uint16
    let endpoint = sprintf "http://localhost:%i" port
