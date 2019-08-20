namespace MordhauBuddy.App.Maps

module View =
    open Fable.Core.JsInterop
    open Fable.React
    open Fable.React.Props
    open Fable.MaterialUI
    open Fable.MaterialUI.Core
    open Fable.MaterialUI.MaterialDesignIcons
    open Fable.MaterialUI.Icons
    open FSharp.Core  /// To avoid shadowing Result<_,_>
    open MordhauBuddy.App
    open RenderUtils
    open RenderUtils.Validation
    open Elmish.React
    open Electron
    open Types

    let private styles (theme : ITheme) : IStyles list = [
        Styles.Custom ("darkList", [
            CSSProp.BackgroundColor theme.palette.background.``default``
        ])
    ]

    let private view' (classes: IClasses) model dispatch =
        div [
            Style [
                CSSProp.FlexDirection "column"
                CSSProp.Display DisplayOptions.Flex
                CSSProp.Height "inherit"
                CSSProp.Padding "5em"
            ]
        ] [
            card [
                Style [ CSSProp.FlexGrow "1" ]
            ] [
                str "Insert logo here"
                typography [] [str (sprintf "MordhauBuddy %s" (Bindings.Info.version))]
                typography [] [str (sprintf "Electron: %s" (Bindings.Info.electronVersion))]
                typography [] [str (sprintf "Chrome: %s" (Bindings.Info.chromeVersion))]
                typography [] [str (sprintf "Node: %s" (Bindings.Info.nodeVersion))]
                typography [] [str (sprintf "V8: %s" (Bindings.Info.v8Version))]
                typography [] [str (sprintf "License: %s" (Bindings.Info.license))]
                typography [] [str (sprintf "Author: %s" (Bindings.Info.author))]

                link [
                    DOMAttr.OnClick <| fun _ -> dispatch (OpenLink(Bindings.Info.homepage))
                ] [ str "Repository" ]
                link [
                    DOMAttr.OnClick <| fun _ -> dispatch (OpenLink(Bindings.Info.issues))
                ] [ str "Issues" ]
                
            ]
        ]

    /// Workaround for using JSS with Elmish
    /// https://github.com/mvsmal/fable-material-ui/issues/4#issuecomment-422781471
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
