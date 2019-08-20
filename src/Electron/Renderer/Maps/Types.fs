namespace MordhauBuddy.App.Maps

module Types =
    open System
    open Fable.Core
    open FSharp.Core  /// To avoid shadowing Result<_,_>
    open MordhauBuddy.App
    open RenderUtils
    open RenderUtils.Directory
    open EngineMods
    open MordhauBuddy.Shared.ElectronBridge
    open Microsoft.FSharp.Reflection

    type Msg =
        | OpenLink of string

    type Model = 
        { Waiting : bool
          MapDir : ConfigDir }
