namespace MordhauBuddy.Shared

module ElectronBridge =
    open FSharp.Reflection

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

        let bPerKb: float<B / KB> = 1000.<B/KB>
        let bPerMb: float<B / MB> = 1000000.<B/MB>
        let kbPerMb: float<KB / MB> = 1000.<KB/MB>
        let mbPerKb: float<MB / KB> = 0.001<MB/KB>
        let gbPerMb: float<GB / MB> = 0.001<GB/MB>
        let convertBtoKB (x: float<B>) = x / bPerKb
        let convertBtoMB (x: float<B>) = x / bPerMb
        let convertKBtoMB (x: float<KB>) = x / kbPerMb
        let convertMBtoKB (x: float<MB>) = x / mbPerKb
        let convertGBtoMB (x: float<GB>) = x / gbPerMb

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
        type MutableStep =
            { Min: MutableValues
              Max: MutableValues
              Step: float }

        type Mutable =
            | MutableStep of MutableStep
            | MutableString

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
        | Input
        member this.Name = this.ToString() + ".ini"

    type OptionGroup =
        { Title: string
          Caption: string
          Settings: KeyValues list
          File: ConfigFile
          Enabled: bool
          Expanded: bool }

    [<RequireQualifiedAccess>]
    type CommunityResult = SteamAnnouncements of (string * string) list

    [<RequireQualifiedAccess>]
    type INIOperationResult =
        | DefaultDir of string option
        | Replace of bool
        | Delete of bool
        | Exists of bool
        | Parse of bool
        | Backup of bool
        | CommitChanges of bool

    type ModInfoFile =
        { ModId: int
          Name: string
          Author: string
          Version: string
          FileId: int
          FileUrl: string
          ImageUrl: string
          ModPage: string
          Replaces: string
          Size: int option
          Info: string option }

        member this.GetMetaData() =
            let desc = this.Replaces |> sprintf "Replaces: %s"
            let author = sprintf "Author: %s" this.Author 

            let fileSize = 
                this.Size 
                |> Option.map (fun s ->
                    (float s) * (1.<B>)
                    |> convertBtoMB
                    |> sprintf "File size: %.1f MB")

            let info = this.Info |> Option.map (sprintf "Info: %s")

            [ desc
              author
              if fileSize.IsSome then fileSize.Value
              if info.IsSome then info.Value ]

    module ModInfoFile =
        let getSize (mi: ModInfoFile) =
            mi.Size
            |> Option.map (fun s ->
                (float s) * (1.<B>)
                |> convertBtoMB)

    [<RequireQualifiedAccess>]
    type ModOperationResult =
        | DefaultDir of string option
        | DirExists of bool
        | Delete of int * Result<bool, string>
        | Disable of int * Result<bool, string>
        | Enable of int * Result<bool, string>

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
    type ModResult =
        | AvailableMods of ModInfoFile list
        | InstalledMods of (ModInfoFile list * ModInfoFile list)
        | InstallMod of int * Result<bool, string>
        | InstallModCancelled of int * bool
        | InstallModProgress of int * int
        | InstallModComplete of int
        | InstallModError of int * string

    [<RequireQualifiedAccess>]
    type SettingResult =
        | EnabledAutoLaunch of bool
        | DisabledAutoLaunch of bool

    [<RequireQualifiedAccess>]
    type UpdateResult =
        | Ready
        | Complete
        | Failed

    [<RequireQualifiedAccess>]
    type MiscResult = MordhauRunning of bool

    [<RequireQualifiedAccess>]
    type BridgeResult =
        | Community of CommunityResult
        | INIOperation of INIOperationResult
        | ModOperation of ModOperationResult
        | Faces of FaceResult
        | Config of ConfigResult
        | Mods of ModResult
        | Settings of SettingResult
        | Updates of UpdateResult
        | Misc of MiscResult

    [<RequireQualifiedAccess>]
    type Caller =
        | Community
        | ModInstaller
        | FaceTools
        | MordhauConfig
        | Settings
        | App

    type Selectors =
        { Selectors: string list }

    type INIFile =
        { File: ConfigFile
          WorkingDir: string option }

    type BackupSettings =
        | KeepAll
        | KeepLast10
        | NoBackups

        member this.Text =
            match this with
            | KeepAll -> "Do not remove any backups"
            | KeepLast10 -> "Keep 10 latest backups"
            | NoBackups -> "No backups - not recommended"

        static member private Cases = FSharpType.GetUnionCases typeof<BackupSettings>

        static member private Instantiate name =
            BackupSettings.Cases
            |> Array.tryFind (fun uc -> uc.Name = name)
            |> Option.map (fun uc -> Reflection.FSharpValue.MakeUnion(uc, [||]) :?> BackupSettings)
            |> Option.get

        static member GetSettings = BackupSettings.Cases |> Array.map (fun uc -> uc.Name |> BackupSettings.Instantiate)

        member this.GetTag =
            BackupSettings.Cases
            |> Seq.tryFind (fun uc -> uc.Name = this.ToString())
            |> Option.map (fun uc -> uc.Tag)
            |> Option.get

        static member GetSettingFromTag(tag: int) =
            BackupSettings.Cases
            |> Seq.tryFind (fun t -> t.Tag = tag)
            |> Option.map (fun uc -> uc.Name |> BackupSettings.Instantiate)
            |> Option.get

        static member TryGetCaseFromText(s: string) =
            BackupSettings.GetSettings
            |> Array.filter (fun setting -> setting.Text = s)
            |> Array.tryHead

        static member TryGetSettingFromText(s: string) =
            BackupSettings.Cases
            |> Seq.tryFind (fun t -> t.Name = s)
            |> Option.map (fun uc -> uc.Name |> BackupSettings.Instantiate)

    type CommunityOperation = GetSteamAnnouncements

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
    type ModFileOperation =
        | DefaultDir
        | DirExists of string
        | Delete of string * int
        | Disable of string * int
        | Enable of string * int

    type Faces =
        | Random of string list
        | Frankenstein of string list
        | Custom of string list * string
        | ProfileList

    type Configs =
        | GetConfigs of OptionGroup list
        | MapConfigs of OptionGroup list

    type SettingsOperation =
        | EnableAutoLaunch
        | DisableAutoLaunch
        | BackupPolicy of BackupSettings
        | SetupLinux

    [<RequireQualifiedAccess>]
    type ModTarget =
        { Directory: string
          ModInfo: ModInfoFile }

    type Mods =
        | GetAvailableMods
        | GetInstalledMods of string
        | InstallMod of ModTarget
        | ConfirmInstalled of int
        | CancelMod of int

    [<RequireQualifiedAccess>]
    type Updates =
        | Start
        | Check

    type MiscOperation = IsMordhauRunning

    type BridgeOperations =
        | CommunityOperation of CommunityOperation
        | INIOperation of INIFileOperation
        | ModOperation of ModFileOperation
        | Faces of Faces
        | Configs of Configs
        | Mods of Mods
        | SettingsOperation of SettingsOperation
        | Updates of Updates
        | Misc of MiscOperation
        static member Endpoint = "/ws"

    type BridgeMsg =
        { Caller: Caller
          File: INIFile option
          BridgeResult: BridgeResult }

    type RemoteClientMsg =
        | Resp of BridgeMsg
        | Connected
        | Disconnected

    type RemoteServerMsg = BridgeOps of BridgeOperations * Caller

    let port = "8085" |> uint16
    let endpoint = sprintf "http://localhost:%i" port
