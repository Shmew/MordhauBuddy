namespace MordhauBuddy.App

open MordhauBuddy.Shared.ElectronBridge

module BridgeUtils =
    [<RequireQualifiedAccess>]
    module INI =
        [<RequireQualifiedAccess>]
        module Ops =
            let private wrapOps iCmd = INIOps(Operation(iCmd))
            let replace oldI newI sels = Replace(oldI,newI,sels) |> wrapOps
            let delete oldI sels = Delete(oldI,sels) |> wrapOps
            let exists iFile = Exists(iFile) |> wrapOps
            let parse iFile = Parse(iFile) |> wrapOps
            let backup iFile = Backup(iFile) |> wrapOps
            let getGameProfiles = DefaultDir |> wrapOps

        [<RequireQualifiedAccess>]
        module Faces =
            let private wrapFace fCmd = INIOps(Faces(fCmd))
            let setRandom profile iVal = Random(profile,iVal) |> wrapFace
            let setFrankenstein profile iVal = Frankenstein(profile,iVal) |> wrapFace
            let setCustom profile iVal fVal = Custom(profile,iVal,fVal) |> wrapFace
            let getProfileList iVal = ProfileList(iVal) |> wrapFace