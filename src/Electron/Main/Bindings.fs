namespace MordhauBuddy.App

module Bindings =
    open Fable.Core
    open Fable.Core.JsInterop
    open Electron

    module Info =
        let private pkgJson: obj = importDefault "../../../package.json"

        let private normalizeKebabCase (s: string) =
            s.Split('-')
            |> Array.map (fun (s: string) ->
                s.Substring(0, 1)
                |> (fun c -> c.ToUpper())
                |> (fun c -> c + s.Substring(1, s.Length)))
            |> Array.reduce (fun acc elem -> acc + " " + elem)

        let version: string = pkgJson?version
        let name: string = pkgJson?name |> normalizeKebabCase
        let homepage: string = pkgJson?homepage
        let description: string = pkgJson?description
        let issues: string = pkgJson?bugs?url
        let author: string = pkgJson?author
        let license: string = pkgJson?license

        type private ElectronVersions =
            abstract chrome: string
            abstract electron: string
            abstract node: string
            abstract v8: string

        [<Emit("process.versions")>]
        let private versions: ElectronVersions = jsNative

        let electronVersion = versions.electron
        let chromeVersion = versions.chrome
        let nodeVersion = versions.node
        let v8Version = versions.v8

    module WindowState =
        type State =

            /// The saved x coordinate of the loaded state. None if the state has not
            /// been saved yet.
            abstract x: int option

            /// The saved y coordinate of the loaded state. None if the state has not
            /// been saved yet.
            abstract y: int

            /// The saved width of loaded state. `defaultWidth` if the state has not
            /// been saved yet.
            abstract width: int

            /// The saved heigth of loaded state. `defaultHeight` if the state has not
            /// been saved yet.
            abstract height: int

            /// `true` if the window state was saved while the window was maximized.
            /// None if the state has not been saved yet.
            abstract isMaximized: bool option

            /// `true` if the window state was saved while the window was in full screen
            /// mode. None if the state has not been saved yet.
            abstract isFullScreen: bool option

            /// Register listeners on the given `BrowserWindow` for events that are
            /// related to size or position changes (`resize`, `move`). It will also
            /// restore the window's maximized or full screen state. When the window is
            /// closed we automatically remove the listeners and save the state.
            abstract manage: window:BrowserWindow -> unit

            /// Removes all listeners of the managed `BrowserWindow` in case it does not
            /// need to be managed anymore.
            abstract unmanage: unit -> unit

            /// Saves the current state of the given `BrowserWindow`. This exists mostly
            /// for legacy purposes, and in most cases it's better to just use `manage`.
            abstract saveState: window:BrowserWindow -> unit

        [<AllowNullLiteral>]
        type Options =

            /// The width that should be returned if no file exists yet.
            /// Defaults to `800`.
            abstract defaultWidth: int with get, set

            /// The height that should be returned if no file exists yet.
            /// Defaults to `600`.
            abstract defaultHeight: int with get, set

            /// The path where the state file should be written to.
            /// Defaults to `app.getPath('userData')`.
            abstract path: string with get, set

            /// The name of file. Defaults to `window-state.json`.
            abstract file: string with get, set

            /// Should we automatically maximize the window, if it was last closed
            /// maximized. Defaults to `true`.
            abstract maximize: bool with get, set

            /// Should we automatically restore the window to full screen, if it was
            /// last closed full screen. Defaults to `true`.
            abstract fullScreen: bool with get, set

        let getState: Options -> State = importDefault "electron-window-state"

    module ElectronUpdater =

        type ILogger =
            abstract debug: message:string -> unit
            abstract error: message:obj -> unit
            abstract info: message:obj -> unit
            abstract warn: message:obj -> unit

        type UpdateInfo =
            abstract version: string
            /// `UpdateFileInfo []`
            abstract files: obj []

            abstract releaseName: string
            /// `ReleaseNoteInfo []` if `fullChangelog` is set to `true`,
            /// `string` otherwise
            abstract releaseNotes: U2<string, obj []>

            abstract releaseDate: string
            /// The staged rollout percentage, 0-100.
            abstract stagingPercentage: float

        type UpdateCheckResult =
            /// Emitted when an authenticating proxy is asking for user credentials.
            abstract updateInfo: UpdateInfo

            abstract downloadPromise: string []
            abstract cancellationToken: obj

        type UpdaterSignal =
            abstract login: username:string * password:string -> unit
            abstract progress: unit -> unit
            abstract updateCancelled: unit -> unit
            abstract updateDownloaded: unit -> unit

        type AutoUpdater =
            inherit EventEmitter<AutoUpdater>
            /// Default: `true`
            ///
            /// Whether to automatically download an update when it is found.
            abstract autoDownload: bool with get, set
            /// Default: `true`
            ///
            /// Whether to automatically install a downloaded update on app
            /// quit (if `quitAndInstall` was not called before).
            abstract autoInstallOnAppQuit: bool with get, set
            /// Default: `false`
            ///
            /// *GitHub provider only.*
            /// Whether to allow update to pre-release versions.
            /// Defaults to `true` if application version contains
            /// prerelease components
            abstract allowPrerelease: bool with get, set
            /// Default: `false`
            ///
            /// *GitHub provider only.*
            /// Get all release notes (from current version to latest),
            /// not just the latest.
            abstract fullChangelog: bool with get, set
            /// Default: `false`
            ///
            /// Whether to allow version downgrade (when a user from
            /// the beta channel wants to go back to the stable channel).
            ///
            /// Taken in account only if channel differs (pre-release version
            /// component in terms of semantic versioning).
            abstract allowDowngrade: bool with get, set
            /// The current application version.
            abstract currentVersion: string
            /// Get the update channel. Not applicable for GitHub. Doesn�t
            /// return channel from the update configuration, only if was previously set.
            abstract channel: string
            /// The request headers
            abstract requestHeaders: string * string with get, set
            /// Logging interface
            abstract logger: ILogger option with get, set
            /// For type safety you can use signals, e.g. `autoUpdater.signals.updateDownloaded(() => {})`
            /// instead of `autoUpdater.on('update-available', () => {})`
            abstract signals: obj with get, set
            /// Asks the server whether there is an update.
            abstract checkForUpdates: unit -> UpdateCheckResult
            /// Asks the server whether there is an update, download and notify if update available.
            abstract checkForUpdatesAndNotify: unit -> unit
            /// Start downloading update manually. You can use this method if `autoDownload`
            /// option is set to `false`. Returns the path to the downloaded file
            abstract downloadUpdate: cancellationToken:obj -> 't
            /// Restarts the app and installs the update after it has been downloaded.
            /// It should only be called after update-downloaded has been emitted.
            ///
            /// *Note:* autoUpdater.quitAndInstall() will close all application windows
            /// first and only emit before-quit event on app after that. This is different
            /// from the normal quit event sequence.
            ///
            /// - isSilent - *windows-only* Runs the installer in silent mode. Defaults to `false`.
            /// - isForceRunAfter - Run the app after finish even on silent install. *Not applicable for macOS*.
            /// Ignored if isSilent is set to `false`.
            abstract quitAndInstall: isSilent:bool * isForceRunAfter:bool -> unit
