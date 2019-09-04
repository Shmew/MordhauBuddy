namespace MordhauBuddy.App.About

module View =
    open Fable.Core.JsInterop
    open Fable.React
    open Fable.React.Props
    open Fable.MaterialUI
    open Fable.MaterialUI.Core
    open Fable.MaterialUI.MaterialDesignIcons
    open Fable.MaterialUI.Icons
    open FSharp.Core /// To avoid shadowing Result<_,_>
    open MordhauBuddy.App
    open RenderUtils
    open RenderUtils.Validation
    open RenderUtils.MaterialUI.Core
    open RenderUtils.MaterialUI.Props
    open RenderUtils.MaterialUI.Themes
    open Elmish.React
    open Electron
    open Types

    let private styles (theme: ITheme): IStyles list =
        [ Styles.Custom("padBottom", [ CSSProp.PaddingBottom "1em" ]) ]

    let private view' (classes: IClasses) model dispatch =
        div
            [ Style
                [ CSSProp.FlexDirection "column"
                  CSSProp.Display DisplayOptions.Flex
                  CSSProp.Height "inherit"
                  CSSProp.Padding "2em" ] ]
            [ card [ 
                CardProp.Raised true
                Style [ 
                    CSSProp.Padding "2em"
                    CSSProp.FlexGrow "1" ] ]
                  [ grid
                      [ GridProp.Container true
                        GridProp.Spacing GridSpacing.``0``
                        GridProp.Justify GridJustify.Center
                        GridProp.AlignItems GridAlignItems.Center 
                        Style [ CSSProp.FlexDirection "column" ] ]
                      [ img
                            [ HTMLAttr.Hidden(model.ImgLoaded |> not)
                              DOMAttr.OnLoad(fun _ -> dispatch ImgSkeleton)
                              HTMLAttr.Src(stat "icon.png")
                              HTMLAttr.Width "200px"
                              HTMLAttr.Height "250px"
                              Style [ 
                                CSSProp.BorderRadius "4px" 
                                CSSProp.PaddingBottom "2em" ] ]
                        skeleton
                            [ HTMLAttr.Hidden <| model.ImgLoaded
                              HTMLAttr.Width "200px"
                              HTMLAttr.Height "250px"
                              SkeletonProp.DisableAnimate true
                              Style [ CSSProp.PaddingBottom "2em" ] ]
                        typography [ Class classes?padBottom ] [ str "Mordhau Buddy" ]
                        typography [ Class classes?padBottom ] [ str (sprintf "Version: %s" (Bindings.Info.version))]
                        typography [ Class classes?padBottom ] [ str (sprintf "Electron: %s" (Bindings.Info.electronVersion)) ]
                        typography [ Class classes?padBottom ] [ str (sprintf "Chrome: %s" (Bindings.Info.chromeVersion)) ]
                        typography [ Class classes?padBottom ] [ str (sprintf "Node: %s" (Bindings.Info.nodeVersion)) ]
                        typography [ Class classes?padBottom ] [ str (sprintf "V8: %s" (Bindings.Info.v8Version)) ]
                        typography [ Class classes?padBottom ] [ str (sprintf "License: %s" (Bindings.Info.license)) ]
                        typography [ Class classes?padBottom ] [ str (sprintf "Author: %s" (Bindings.Info.author)) ]
                        link [
                            Class classes?padBottom 
                            DOMAttr.OnClick <| fun _ -> dispatch (OpenLink(Bindings.Info.homepage)) 
                        ] [ typography [] [ str "Repository" ] ]
                        link [ 
                            Class classes?padBottom
                            DOMAttr.OnClick <| fun _ -> dispatch (OpenLink(Bindings.Info.issues)) 
                        ] [ typography [] [ str "Issues" ] ]
                    ] ] ]

    /// Workaround for using JSS with Elmish
    /// https://github.com/mvsmal/fable-material-ui/issues/4#issuecomment-422781471
    type private IProps =
        abstract model: Model with get, set
        abstract dispatch: (Msg -> unit) with get, set
        inherit IClassesProps

    type private Component(p) =
        inherit PureStatelessComponent<IProps>(p)
        let viewFun (p: IProps) = view' p.classes p.model p.dispatch
        let viewWithStyles = withStyles (StyleType.Func styles) [] viewFun
        override this.render() = ReactElementType.create viewWithStyles this.props []

    let view (model: Model) (dispatch: Msg -> unit): ReactElement =
        let props =
            jsOptions<IProps> (fun p ->
                p.model <- model
                p.dispatch <- dispatch)
        ofType<Component, _, _> props []
