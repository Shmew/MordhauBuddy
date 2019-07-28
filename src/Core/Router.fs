namespace MordhauBuddy.Core

open ElectronCgi.DotNet
open FileOps
open INIReader
open INIReader.INIExtensions.Options
open System

module Router =
    [<EntryPoint>]
    let main args =
        let conn = (new ConnectionBuilder()).WithLogging().Build()
        conn.On<string, string>("test", (fun data -> "test" + data))
        conn.Listen()
        0
