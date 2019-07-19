namespace MordhauBuddy.Core

open System
open INIReader
open INIReader.INIExtensions.Options

/// Operations on Mordhau Game ini
module Game =
    /// Module for modifying character face values
    module Frankenstein =
        let max = 65535
        let min = 0
        let rng = Random()
        /// Generate random values and map over `int list`
        let random (faceValues : int list) = faceValues |> List.map (fun _ -> rng.Next(min, max))

        /// Randomly assign max or min values over `int list`
        let frankensteinRandom (faceValues : int list) =
            faceValues
            |> List.map (fun _ ->
                   if rng.Next(min, 1) = 1 then max
                   else min)

/// Operations on Mordhau Engine ini
module Engine =
    let getSystemSettings (engineFile: INIValue) = 
        match engineFile?SystemSettings with
        | Some(iList) -> Some(iList)
        | None -> None

    let modifySectionSettings (engineFile: INIValue) (section: string) (filters: string list) (iVal: INIValue) =
        engineFile.Map(section::filters, iVal)

    let addSectionSettings (engineFile: INIValue) (section: string) (values: INIValue list) =
        match engineFile.TryGetProperty(section) with
        | Some(s) ->
            s |> List.append values |> List.sort
            |> (fun newVal -> engineFile.Map([section],INIValue.Section(section,newVal)))
        | None ->
            (section,values |> List.sort)::engineFile.Properties
            |> List.map INIValue.Section
            |> INIValue.File
