module MichaelGlass.FSharp.Analyzers.Tests.ErrorReportingAnalyzerTests

open Xunit
open Swensen.Unquote
open FSharp.Analyzers.SDK
open MichaelGlass.FSharp.Analyzers.Tests.Common
open MichaelGlass.FSharp.Analyzers.ErrorReportingAnalyzer

let private requiredFunctions = Set.ofList [ "logError"; "captureError" ]

[<Fact>]
let ``flags try/with missing error reporting call`` () =
    let source = readTestData [ "error-reporting"; "MissingErrorReport.fs" ]
    let context = getContextForSource source
    let messages = analyze requiredFunctions context

    test <@ messages.Length = 1 @>
    test <@ messages.[0].Code = "MGA-ERROR-REPORT-001" @>
    test <@ messages.[0].Severity = Severity.Warning @>

[<Fact>]
let ``does not flag try/with that calls error reporting function`` () =
    let source = readTestData [ "error-reporting"; "HasErrorReport.fs" ]
    let context = getContextForSource source
    let messages = analyze requiredFunctions context

    test <@ messages.Length = 0 @>

[<Fact>]
let ``multi-clause try/with emits exactly one diagnostic`` () =
    let source = readTestData [ "error-reporting"; "MultiClauseMissingReport.fs" ]
    let context = getContextForSource source
    let messages = analyze requiredFunctions context

    test <@ messages.Length = 1 @>
    test <@ messages.[0].Code = "MGA-ERROR-REPORT-001" @>

[<Fact>]
let ``returns empty when no required functions configured`` () =
    let source = readTestData [ "error-reporting"; "no-config"; "NoConfig.fs" ]
    let context = getContextForSource source
    let messages = analyze Set.empty context

    test <@ messages.Length = 0 @>
