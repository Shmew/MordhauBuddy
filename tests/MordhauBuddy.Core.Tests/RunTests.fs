namespace MordhauBuddy.Core.Tests

open Expecto

module RunTests =
    [<EntryPoint>]
    let main args =
        let writeResults = TestResults.writeNUnitSummary (@"bin\TestResults.xml", "Expecto.Tests")
        let config = defaultConfig.appendSummaryHandler writeResults
        Tests.runTestsInAssembly config args
