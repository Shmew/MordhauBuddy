namespace MordhauBuddy.Core.Tests

module HelperTests =
    open Expecto
    open FSharp.Data
    open FSharp.Data.JsonExtensions
    open MordhauBuddy.Core
    open System

    let cmkvPair = testList "INI Parses correctly" [ testCase "Parses engine" <| fun () -> Expect.isSome (Some("")) "" ]

    [<Tests>]
    let runHelperTests = testList "Helpers" [ cmkvPair ]
