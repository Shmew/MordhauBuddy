namespace MordhauBuddy.Shared

module ElectronBridge =
    type RemoteServerMsg =
        | Text of string
        | Close

    type RemoteClientMsg = Resp of string

    let port = "8085" |> uint16
    let endpoint = sprintf "http://localhost:%i" port
    let socketPath = "/ws"
