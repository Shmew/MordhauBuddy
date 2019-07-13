namespace MordhauBuddy.Core

open FSharp.Data
open INIReader
open INIReader.INIExtensions
open INIReader.INIExtensions.Options
open System.IO

module Lib =
    let ast =
        match File.Exists(@"C:\Users\Shmew\source\repos\MordhauBuddy\Game.ini") with
        | true ->
            //File.ReadAllTextAsync(@"C:\Users\Shmew\source\repos\MordhauBuddy\Game.ini")
            //|> Async.AwaitTask
            //|> Async.RunSynchronously
            //|> INIValue.TryParse
            File.ReadAllText(@"C:\Users\Shmew\source\repos\MordhauBuddy\Game.ini") |> INIValue.Parse
        | false -> failwith "Unable to parse file."

    let result = ast.GetProperty(@"/Game/Mordhau/Blueprints/BP_MordhauSingleton.BP_MordhauSingleton_C")

    File.WriteAllText(@"C:\temp\test.ini", ast.ToString())
