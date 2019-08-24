namespace MordhauBuddy.App.MapsInstaller

module Types =
    open FSharp.Core  /// To avoid shadowing Result<_,_>
    open MordhauBuddy.App
    open RenderUtils
    open RenderUtils.Directory
    open RenderUtils.WebParsing
    open RenderUtils.MapTypes
    open MordhauBuddy.Shared.ElectronBridge
    open Microsoft.FSharp.Reflection

    type Tab =
        | Available
        | Installed
        | Installing
        member this.Text =
            this.ToString()
            |> String.duToTitle

        static member private Cases =
            FSharpType.GetUnionCases typeof<Tab>

        static member private Instantiate name =
            Tab.Cases
            |> Array.tryFind (fun uc -> uc.Name = name)
            |> Option.map (fun uc -> 
                Reflection.FSharpValue.MakeUnion( uc, [||] ) :?> Tab)
            |> Option.get

        static member GetTabs =
            Tab.Cases
            |> Array.map (fun uc ->
                uc.Name |> Tab.Instantiate)

        member this.GetTag =
            Tab.Cases
            |> Seq.tryFind (fun uc -> uc.Name = this.ToString())
            |> Option.map (fun uc -> uc.Tag)
            |> Option.get

        static member GetTabFromTag (tag: int) =
            Tab.Cases
            |> Seq.tryFind (fun t -> t.Tag = tag)
            |> Option.map (fun uc -> uc.Name |> Tab.Instantiate)
            |> Option.get

    type Msg =
        | ClientMsg of BridgeMsg
        | TabSelected of Tab
        | ImgSkeleton
        | Install of string * string
        | InstallAll
        | Uninstall of string
        | CancelInstall of string
        | Update of string
        | GetInstalled
        | GetAvailable
        | Refresh
        | SnackMsg of Snackbar.Types.Msg<Msg>
        | SnackDismissMsg

    type CommunityMapWithProgress =
        { Map : CommunityMap
          Progress : int
          Error : bool
          HelperText : string }
        static member Init (map : CommunityMap) =
            { Map = map
              Progress = 0
              Error = false
              HelperText = "" }

    type Model = 
        { Waiting : bool
          MapsDir : ConfigDir
          Available : CommunityMap list
          Installed : CommunityMap list
          Installing : CommunityMapWithProgress list
          TabSelected : Tab
          Snack : Snackbar.Types.Model<Msg> }
