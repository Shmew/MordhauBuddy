namespace MordhauBuddy.Core

open System

module Frankenstein =
    let private max = 65535
    let private min = 0
    let rng = Random()
    let random (faceValues : int list) = faceValues |> List.map (fun _ -> rng.Next(min, max))

    let frankensteinRandom (faceValues : int list) =
        faceValues
        |> List.map (fun _ ->
               if rng.Next(min, 1) = 1 then max
               else min)
