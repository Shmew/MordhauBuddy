namespace MordhauBuddy.App

module Main =
    open Fable.Core
    open Fable.Core.JsInterop
    open Fable.Import
    //open MordhauBuddy.Bindings.Electron
    open Electron
    open Node.Api
    open Bindings

    // A global reference to the window object is required in order to prevent garbage collection
    let mutable mainWindow : BrowserWindow option = Option.None
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


    let bridgePath =
#if DEBUG
        path.resolve (__dirname, "..", "../../bin/Core/netcoreapp3.0/Core.exe")
#else
        path.resolve (__dirname, @"Core.exe")
#endif


    let startBridge() =
        let bridgeProc = childProcess.execFile (bridgePath, callback = (fun _ _ _ -> ()))
#if DEBUG
        bridgeProc.stdout.on ("data",
                              (fun data ->
                              if mainWindow.IsSome then JS.console.log data))
        |> ignore
#endif

        bridgeProc

    let bridge = startBridge()

    let createMainWindow() =
        let mainWinState =
            WindowState.getState (jsOptions<WindowState.Options> (fun o ->
                                      o.defaultHeight <- 925
                                      o.defaultWidth <- 1200))

        let win =
            main.BrowserWindow.Create(jsOptions<BrowserWindowOptions> (fun o ->
                                          o.width <- mainWinState.width
                                          o.height <- mainWinState.height
                                          o.minHeight <- 925
                                          o.minWidth <- 1200
                                          o.autoHideMenuBar <- true
                                          o.webPreferences <- jsOptions<WebPreferences> (fun w ->
                                                                  w.contextIsolation <- false
                                                                  w.nodeIntegration <- true)
                                          o.frame <- false
                                          o.backgroundColor <- "#FFF"
                                          o.show <- false))

        //let ctxMenu = main.Menu.Create()
        //ctxMenu.append (jsOptions<MenuItemOptions> (fun o -> o.label <- "Hello") |> main.MenuItem.Create)
        //win.webContents.on ("context-menu", (fun ev -> ctxMenu.popup())) |> ignore
        win.onceReadyToShow (fun _ ->
            win.setTitle <| sprintf "%s - %s" Info.name Info.version
            win.show()
            mainWinState.manage win)
        |> ignore
#if DEBUG
        // Set up dev tools
        DevTools.installAllDevTools win |> ignore
        DevTools.connectRemoteDevViaExtension()
        // Open dev tools on startup
        win.webContents.openDevTools()
        // Load correct URL
        win.loadURL (sprintf "http://localhost:%s" ``process``.env?ELECTRON_WEBPACK_WDS_PORT) |> ignore
        ``process``.on ("uncaughtException", (fun err -> JS.console.log (err.ToString()))) |> ignore
#else
        path.join(__dirname, "index.html")
        |> sprintf "file:///%s"
        |> win.loadURL
        |> ignore
#endif

        //let copy : U2<MenuItemOptions, MenuItem> =
        //    jsOptions<Electron.MenuItemOptions> (fun o ->
        //        o.label <- "Test"
        //        o.role <- MenuItemRole.Copy)
        //    |> U2.Case1
        //|> main.MenuItem.Create
        //main.Menu.buildFromTemplate [| copy |] |> win.setMenu /// DOESN"T SEEM TO WORK
        // Dereference the window object when closed. If your app supports
        // multiple windows, you can store them in an array and delete the
        // corresponding element here.
        win.onClosed (fun _ -> mainWindow <- None) |> ignore
        mainWindow <- Some win

    // This method will be called when Electron has finished
    // initialization and is ready to create browser windows.
    main.app.onReady (fun _ _ -> createMainWindow()) |> ignore
    // Quit when all windows are closed.
    main.app.onWindowAllClosed (fun _ ->
        // On OS X it's common for applications and their menu bar
        // to stay active until the user quits explicitly with Cmd + Q
        if ``process``.platform <> Node.Base.Platform.Darwin then main.app.quit())
    |> ignore
    main.app.onActivate (fun _ _ ->
        // On OS X it's common to re-create a window in the app when the
        // dock icon is clicked and there are no other windows open.
        if mainWindow.IsNone then createMainWindow())
    |> ignore
//mainWindow.Value.setProgressBar
