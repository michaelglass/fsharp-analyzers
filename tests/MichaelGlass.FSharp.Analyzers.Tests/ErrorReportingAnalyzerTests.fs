module MichaelGlass.FSharp.Analyzers.Tests.ErrorReportingAnalyzerTests

open Xunit
open Swensen.Unquote
open FSharp.Analyzers.SDK
open FSharp.Analyzers.SDK.Testing
open MichaelGlass.FSharp.Analyzers.ErrorReportingAnalyzer

let private projectOptions =
    mkOptionsFromProject "net10.0" []
    |> Async.AwaitTask
    |> Async.RunSynchronously

let private getContextForSource (source: string) =
    getContext projectOptions source

let private requiredFunctions = Set.ofList [ "logError"; "captureError" ]

[<Fact>]
let ``flags try/with missing error reporting call`` () =
    let source =
        System.IO.File.ReadAllText(
            System.IO.Path.Combine(__SOURCE_DIRECTORY__, "data", "error-reporting", "MissingErrorReport.fs")
        )

    let context = getContextForSource source
    let messages = analyze requiredFunctions context

    test <@ messages.Length = 1 @>
    test <@ messages.[0].Code = "MGA-ERROR-REPORT-001" @>
    test <@ messages.[0].Severity = Severity.Warning @>

[<Fact>]
let ``does not flag try/with that calls error reporting function`` () =
    let source =
        System.IO.File.ReadAllText(
            System.IO.Path.Combine(__SOURCE_DIRECTORY__, "data", "error-reporting", "HasErrorReport.fs")
        )

    let context = getContextForSource source
    let messages = analyze requiredFunctions context

    test <@ messages.Length = 0 @>

[<Fact>]
let ``multi-clause try/with emits exactly one diagnostic`` () =
    let source =
        System.IO.File.ReadAllText(
            System.IO.Path.Combine(__SOURCE_DIRECTORY__, "data", "error-reporting", "MultiClauseMissingReport.fs")
        )

    let context = getContextForSource source
    let messages = analyze requiredFunctions context

    test <@ messages.Length = 1 @>
    test <@ messages.[0].Code = "MGA-ERROR-REPORT-001" @>

[<Fact>]
let ``returns empty when no required functions configured`` () =
    let source =
        System.IO.File.ReadAllText(
            System.IO.Path.Combine(__SOURCE_DIRECTORY__, "data", "error-reporting", "no-config", "NoConfig.fs")
        )

    let context = getContextForSource source
    let messages = analyze Set.empty context

    test <@ messages.Length = 0 @>
