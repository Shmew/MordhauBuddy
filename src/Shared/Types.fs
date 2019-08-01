namespace MordhauBuddy.Shared

module ElectronBridge =
    [<AutoOpen>]
    module INITypes =
        type Selectors =
            { Selectors: string list }

        type INIFile =
            { File: string
              WorkingDir: string option }

        [<RequireQualifiedAccess>]
        [<StructuredFormatDisplay("{_Print}")>]
        type INIValue =
            | String of string option
            | FieldText of string * INIValue
            | Tuple of INIValue list
            | KeyValue of string * INIValue
            | Section of string * INIValue list
            | File of INIValue list

            /// [omit]
            [<CompilerMessageAttribute("This method is intended for use in generated code only.", 10001, IsHidden = true,
                                       IsError = false)>]
            member x._Print =
                match x.ToString() with
                | str when str.Length > 512 -> str.Substring(0, 509) + "..."
                | str -> str

        type FaceValues =
            { Translate: int list
              Rotate: int list
              Scale: int list }
            member this.getTuples =
                [this.Translate; this.Rotate; this.Scale]
                |> List.map (fun iList ->
                    iList 
                    |> List.map (fun i ->
                        i |> string 
                        |> Some 
                        |> INIValue.String) 
                    |> INIValue.Tuple)

        type FileOperation =
            | Replace of INIValue * INIValue * Selectors
            | Delete of INIValue * Selectors
            | Exists of INIFile
            | Parse of INIFile
            | Backup of INIFile
            | DefaultDir

        type Faces =
            | Random of string * INIValue
            | Frankenstein of string * INIValue
            | Custom of string * INIValue * FaceValues
            | ProfileList of INIValue

        type INIOperations =
            | Operation of FileOperation
            | Faces of Faces

    type RemoteServerMsg =
        | INIOperations of INIOperations

    [<RequireQualifiedAccess>]
    type BridgeResult =
        | IValue of INIValue
        | TextList of string list
        | Text of string
        | Success
        | Failure

    type RemoteClientMsg = 
        | Resp of BridgeResult

    let port = "8085" |> uint16
    let endpoint = sprintf "http://localhost:%i" port
    let socketPath = "/ws"
