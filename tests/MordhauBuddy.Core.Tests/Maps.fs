namespace MordhauBuddy.Core.Tests

module Maps =
    open Expecto
    open MordhauBuddy.Core
    open MordhauBuddy.Shared.ElectronBridge
    open Maps
    open Helpers
    open System
    open FSharp.Data
    open FSharp.Data.JsonExtensions

    let http =
        [ testCase "paramBuilder constructs a proper string" <| fun () ->
            let result =
                Http.paramBuilder
                    ([ Param(("name", Some("wow")))
                       Param(("folder", None))
                       Flag("dothing", true)
                       Flag("really", false)
                       Param(("maybe", Some("yes"))) ])
            Expect.equal result "?name=wow&dothing&maybe=yes" ""
          testCase "httpErrorMsg parses the error response" <| fun () ->
              let result =
                  let json =
                      """{"errors": [{"col": 0,"file": "string","id": "string","item": "string","line": 0,"message": "Some random message"}]}"""
                  { Body = HttpResponseBody.Text(json)
                    StatusCode = 500
                    ResponseUrl = ""
                    Headers = new Map<string, string>(Seq.empty)
                    Cookies = new Map<string, string>(Seq.empty) }
                  |> Http.httpErrorMsg
              Expect.equal result "Some random message" "" ]
        |> testList "Http"

    [<Tests>]
    let tests = testList "Maps" [ http ]
