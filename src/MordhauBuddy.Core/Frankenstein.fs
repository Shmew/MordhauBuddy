namespace MordhauBuddy.Core

open FSharp.Data
open INIReader
open System

module Lib =
    let ast = INIParser.readFile @"C:\Users\Shmew\source\repos\MordhauBuddy\Game.ini"
