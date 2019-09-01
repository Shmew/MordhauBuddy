namespace MordhauBuddy.App.Settings

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
    open MordhauBuddy.Shared.ElectronBridge
    open RenderUtils
    open RenderUtils.Validation
    open RenderUtils.Directory
    open Elmish.React
    open Electron
    open Types

    let private styles (theme: ITheme): IStyles list =
        [ Styles.Custom("darkList", [ CSSProp.BackgroundColor theme.palette.background.``default`` ]) ]

    let private dirConfig (classes: IClasses) model dispatch (dir: ConfigDir) =
        div
            [ Style
                [ CSSProp.Padding "2em"
                  CSSProp.Display DisplayOptions.Flex
                  CSSProp.MinHeight "76px" ] ]
            [ textField
                [ TextFieldProp.Variant TextFieldVariant.Outlined
                  MaterialProp.FullWidth true
                  HTMLAttr.Label dir.Label
                  HTMLAttr.Value dir.Directory
                  MaterialProp.Error dir.State.IsDirError
                  match dir.State with
                  | DirState.Init s
                  | DirState.Error s
                  | DirState.Success s -> s
                  | _ -> ""
                  |> fun s -> TextFieldProp.HelperText(s |> str) ] []
              button
                  [ ButtonProp.Variant ButtonVariant.Contained
                    MaterialProp.Color ComponentColor.Secondary
                    DOMAttr.OnClick <| fun _ -> dispatch (RequestLoad(dir.Dir))
                    Style
                        [ CSSProp.MarginLeft "1em"
                          CSSProp.MaxHeight "4em" ] ] [ str "Select" ] ]

    let private selectMenuItems opt =
        menuItem 
            [ HTMLAttr.Value opt
              Prop.Key opt ]
            [ div 
                []
                [ typography [] [ str opt ] ] ]

    let private selectForm (classes: IClasses) model dispatch (inputL: ReactElement) (selects: ReactElement) =
        div
            [ Style
                [ CSSProp.Padding "2em"
                  CSSProp.Display DisplayOptions.Flex
                  CSSProp.MinHeight "8em" ] ]
            [
            form 
                [ DOMAttr.OnSubmit <| fun ev -> ev.preventDefault() ]
                [ formControl
                    [ MaterialProp.FullWidth true
                      FormControlProp.Variant FormControlVariant.Outlined
                      Style
                        [ CSSProp.MinWidth "30em" ]]
                    [ inputL; selects ] ] ]

    let private selectUpdate (classes: IClasses) model dispatch =
        let inputL = inputLabel [] [ str "Map update settings" ] 
        select
            [ HTMLAttr.Value model.MapUpdateSettings.Text
              DOMAttr.OnChange <| fun ev -> ev.Value |> UpdateSettings.TryGetCaseFromText |> MapUpdateSetting |> dispatch 
              SelectProp.Variant SelectVariant.Outlined 
              SelectProp.Input <| outlinedInput [ OutlinedInputProp.LabelWidth 150 ] [] ]
            (UpdateSettings.GetSettings |> Array.map (fun s -> selectMenuItems s.Text))
        |> selectForm classes model dispatch inputL

    let private selectBackup (classes: IClasses) model dispatch =
        let inputL = inputLabel [] [ str "Backup settings" ] 
        select
            [ HTMLAttr.Value model.BackupSettings.Text
              DOMAttr.OnChange <| fun ev -> ev.Value |> BackupSettings.TryGetCaseFromText |> BackupSetting |> dispatch 
              SelectProp.Variant SelectVariant.Outlined 
              SelectProp.Input <| outlinedInput [ OutlinedInputProp.LabelWidth 120 ] [] ]
            (BackupSettings.GetSettings |> Array.map (fun s -> selectMenuItems s.Text))
        |> selectForm classes model dispatch inputL

    let private toggleAutoLaunch (classes: IClasses) model dispatch =
        formGroup [] [
            formControlLabel [
                FormControlLabelProp.Control <|
                    checkbox
                        [ DOMAttr.OnClick <| fun _ -> (dispatch ToggleAutoLaunch)
                          HTMLAttr.Checked(model.AutoLaunch) ]
                FormControlLabelProp.LabelPlacement FormControlLabelPlacement.Top
                HTMLAttr.Label "Launch application on startup"
            ] []
        ]

    let private view' (classes: IClasses) model dispatch =
        let dConf (dir: ConfigDir) = dirConfig classes model dispatch dir
        div
            [ Style
                [ CSSProp.FlexDirection "column"
                  CSSProp.Display DisplayOptions.Flex
                  CSSProp.Height "inherit"
                  CSSProp.Padding "2em" ] ]
            [ card [ Style [ CSSProp.FlexGrow "1" ] ]
                  [ dConf model.GameDir
                    dConf model.EngineDir
                    dConf model.GameUserDir
                    dConf model.MapsDir 
                    selectUpdate classes model dispatch
                    selectBackup classes model dispatch
                    toggleAutoLaunch classes model dispatch] ]

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
