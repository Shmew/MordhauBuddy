namespace MordhauBuddy.App

module Main =
    open MordhauBuddy.Electron
    open MordhauBuddy.ElectronHelpers
    open Fable.Core
    open Fable.Core.JsInterop
    open Fable.Import
    open Node.Api
    open Utils

    // A global reference to the window object is required in order to prevent garbage collection
    let mutable mainWindow : BrowserWindow option = Option.None

    [<Emit("$0.webContents.focus()")>]
    let webContentsFocus (win : BrowserWindow) : unit = jsNative
#if DEBUG

    module DevTools =
        let private installDevTools (extensionRef : obj) (forceDownload : bool) : JS.Promise<string> =
            importDefault "electron-devtools-installer"
        let private REACT_DEVELOPER_TOOLS : obj = import "REACT_DEVELOPER_TOOLS" "electron-devtools-installer"
        let private REDUX_DEVTOOLS : obj = import "REDUX_DEVTOOLS" "electron-devtools-installer"

        let private installDevTool extensionRef =
            promise {
                try
                    let! name = installDevTools extensionRef false
                    JS.console.log ("Added extension", name)
                with err -> JS.console.log ("An error occurred adding extension:", err)
            }
            |> ignore

        let installAllDevTools (win : BrowserWindow) =
            installDevTool REACT_DEVELOPER_TOOLS
            installDevTool REDUX_DEVTOOLS

        let uninstallAllDevTools (win : BrowserWindow) =
            main.BrowserWindow.removeDevToolsExtension ("React Developer Tools")
            main.BrowserWindow.removeDevToolsExtension ("Redux DevTools")

        let connectRemoteDevViaExtension : unit -> unit = import "connectViaExtension" "remotedev"
#endif


    let store = ElectronStore.getStore.Create()

    let createMainWindow() =
        let winStateOpts =
            jsOptions<WindowState.Options> (fun o ->
                o.defaultHeight <- Some 700
                o.defaultWidth <- Some 1200)

        let mainWinState = WindowState.getState winStateOpts

        let options =
            jsOptions<BrowserWindowOptions> (fun o ->
                o.width <- mainWinState.width
                o.height <- mainWinState.height
                o.autoHideMenuBar <- true
                o.webPreferences <- jsOptions<WebPreferences> (fun w ->
                                        w.contextIsolation <- false
                                        w.nodeIntegration <- true)
                o.frame <- false
                o.backgroundColor <- "#FFF"
                o.show <- false)

        let win = main.BrowserWindow.Create(options)

        let onLoad (browser : BrowserWindow) =
            browser.setTitle <| sprintf "%s - %s" Info.name Info.version
            browser.show()
        win.once ("ready-to-show", fun _ -> onLoad win) |> ignore
        mainWinState.manage win
#if DEBUG
        // Set up dev tools
        DevTools.installAllDevTools win |> ignore
        DevTools.connectRemoteDevViaExtension()
        // Open dev tools on startup
        jsOptions<OpenDevToolsOptions> (fun o -> o.activate <- true)
        |> (fun o -> win.webContents.openDevTools (options = o))
        // Load correct URL
        let port = ``process``.env?ELECTRON_WEBPACK_WDS_PORT
        win.loadURL (sprintf "http://localhost:%s" port) |> ignore
#else
        path.join(__dirname, "index.html")
        |> sprintf "file:///%s"
        |> win.loadURL
        |> ignore
#endif

        // Dereference the window object when closed. If your app supports
        // multiple windows, you can store them in an array and delete the
        // corresponding element here.
        win.on ("closed", fun ev -> mainWindow <- Option.None) |> ignore
        win.webContents.on ("devtools-opened", fun ev -> webContentsFocus win) |> ignore
        mainWindow <- Some win

    // This method will be called when Electron has finished
    // initialization and is ready to create browser windows.
    main.app.onReady (fun _ _ -> createMainWindow()) |> ignore
    // Quit when all windows are closed.
    main.app.onWindowAllClosed (fun ev ->
        // On OS X it's common for applications and their menu bar
        // to stay active until the user quits explicitly with Cmd + Q
        if ``process``.platform <> Node.Base.Platform.Darwin then main.app.quit())
    |> ignore
    main.app.onActivate (fun ev _ ->
        // On OS X it's common to re-create a window in the app when the
        // dock icon is clicked and there are no other windows open.
        if mainWindow.IsNone then createMainWindow())
    |> ignore
//mainWindow.Value.setProgressBar
