namespace MordhauBuddy.App

module INITest =
    open System
    open Fable.Core
    open Fable.Core.JsInterop
    open Fable.React
    open Fable.React.Props
    open Fable.MaterialUI
    open Fable.MaterialUI.Core
    open Fable.MaterialUI.Props
    open Fable.MaterialUI.MaterialDesignIcons
    open Fable.MaterialUI.Icons
    open FSharp.Core
    open Utils
    open Elmish
    open Elmish.Bridge
    open MordhauBuddy.Shared.ElectronBridge
    open Toastr

    //type FileItem =
    //    | Directory of {| Id: int; Name: string; IsOpen: bool; Children: FileItem list |}
    //    | File of {| Id: int; Name: string |}

    type State = { Message: string }

    type Model = { State : State }

    type Msg =
        | ToggleDirectory of int
        | ServerMsg of RemoteServerMsg
        | ClientMsg of string

    let init() = {
        //Files = [
        //    Directory 
        //        {| Id = 1
        //           Name = "Documents"
        //           IsOpen = false 
        //           Children = [
        //            File {| Id = 2; Name = "report.pdf" |}
        //            File {| Id = 3; Name = "image.png" |}
        //            Directory 
        //                {| Id = 4
        //                   Name = "Programs"
        //                   IsOpen = false
        //                   Children = [ File {| Id = 5; Name = "word.exe" |} ] |}
        //        ]
        //    |}
        //]
        State = {Message = "init state"}
    }

    //myCmd "test" (fun _ result -> System.Console.WriteLine(result.ToString()))

    //let rec toggleDirectoryOpened id = function
    //    | File file -> File file
    //    | Directory directory when directory.Id = id ->
    //        Directory {| directory with IsOpen = not directory.IsOpen |}
    //    | Directory directory ->
    //        Directory {| directory with Children = List.map (toggleDirectoryOpened id) directory.Children |}

    //let update (msg: Msg) (model: Model) =
    //    match msg with
    //    | ToggleDirectory id -> { model with Files = List.map (toggleDirectoryOpened id) model.Files }

    let update (msg: Msg) (model: Model) =
        match msg with
        | ToggleDirectory id -> 
            { model with State = {Message = "updater"} }
        | ServerMsg sMsg ->
            model
            //Bridge.NamedSend("testBridge",bMsg, callback = (fun () -> ()))
        | ClientMsg cMsg ->
            { model with State = {Message = cMsg} }

    //let fileIcon = i [ Class "fa fa-file" ] [ ]
    //let openFolderIcon = i [ Class "fa fa-folder-open" ] [ ]
    //let closedFolderIcon = i [ Class "fa fa-folder" ] [ ]

    //let rec renderFile dispatch = function
    //    | File file ->
    //        AnimatedTree.animatedTree [
    //            AnimatedTree.Key file.Id
    //            AnimatedTree.Icon fileIcon
    //            AnimatedTree.Content (str file.Name)
    //        ]
    //    | Directory directory ->
    //        AnimatedTree.animatedTree [
    //            AnimatedTree.Key directory.Id
    //            AnimatedTree.Icon (if directory.IsOpen then openFolderIcon else closedFolderIcon)
    //            AnimatedTree.Content (str directory.Name)
    //            AnimatedTree.IsOpen directory.IsOpen
    //            AnimatedTree.OnToggled (fun _ -> dispatch (ToggleDirectory directory.Id))
    //            AnimatedTree.Children [ for file in directory.Children -> renderFile dispatch file  ]
    //        ]


    // Domain/Elmish above, view below
    let private styles (theme : ITheme) : IStyles list = []

    let private view' (classes: IClasses) model dispatch =
        div [] [
            str (model.State.Message)
            iconButton [
                DOMAttr.OnClick (fun _ -> dispatch (ServerMsg(Text("wow"))))
            ] [ str "Send msg to server" ]
        ]

    // Workaround for using JSS with Elmish
    // https://github.com/mvsmal/fable-material-ui/issues/4#issuecomment-422781471
    type private IProps =
        abstract model : Model with get, set
        abstract dispatch : (Msg -> unit) with get, set
        inherit IClassesProps

    type private Component(p) =
        inherit PureStatelessComponent<IProps>(p)
        let viewFun (p : IProps) = view' p.classes p.model p.dispatch
        let viewWithStyles = withStyles (StyleType.Func styles) [] viewFun
        override this.render() = ReactElementType.create viewWithStyles this.props []
            
    let view (model : Model) (dispatch : Msg -> unit) : ReactElement =
        let props =
            jsOptions<IProps> (fun p ->
                p.model <- model
                p.dispatch <- dispatch)
        ofType<Component, _, _> props []
