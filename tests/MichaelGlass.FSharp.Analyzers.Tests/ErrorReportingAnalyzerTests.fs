module MichaelGlass.FSharp.Analyzers.Tests.ErrorReportingAnalyzerTests

open System.IO
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

// --- Suppression placement, end to end ----------------------------------------
// These sources are inline rather than files under data/ because the whole point
// is the exact line placement of the marker, and `mise run format` rewrites every
// .fs file under tests/ — including the fixtures.

[<Fact>]
let ``suppresses when the marker is written above its own justification`` () =
    let source =
        """module TestData.MarkerAboveJustification

let riskyOperation () =
    // MGA-ERROR-REPORT-001:ok - a part we cannot decode contributes no fields;
    // the caller's report is still recorded from whatever else parsed.
    try
        failwith "boom"
    with ex ->
        printfn "caught: %A" ex
"""

    let context = getContextForSource source
    let messages = analyze requiredFunctions context

    test <@ messages.Length = 0 @>

[<Fact>]
let ``still flags a try-with whose marker is detached by a blank line`` () =
    let source =
        """module TestData.MarkerDetachedByBlankLine

let riskyOperation () =
    // MGA-ERROR-REPORT-001:ok - stale marker, left behind by an earlier edit.

    try
        failwith "boom"
    with ex ->
        printfn "caught: %A" ex
"""

    let context = getContextForSource source
    let messages = analyze requiredFunctions context

    test <@ messages.Length = 1 @>
    test <@ messages.[0].Code = "MGA-ERROR-REPORT-001" @>

[<Fact>]
let ``still flags a try-with whose marker is detached by a line of code`` () =
    let source =
        """module TestData.MarkerDetachedByCode

let riskyOperation () =
    // MGA-ERROR-REPORT-001:ok - stale marker, left behind by an earlier edit.
    let attempts = 1
    try
        failwith "boom"
    with ex ->
        printfn "caught: %A (%d)" ex attempts
"""

    let context = getContextForSource source
    let messages = analyze requiredFunctions context

    test <@ messages.Length = 1 @>
    test <@ messages.[0].Code = "MGA-ERROR-REPORT-001" @>

// --- Configuration that changes under a long-lived process -------------------------
// The whole analyzer, through its real CLI entry point, in one process: exactly the
// shape of an analyzer host that stays up across an .editorconfig edit. The host
// re-runs the analyzer because its own cache key covers the config chain; this test
// asks whether the ANSWER that comes back is computed from the config on disk now,
// or from the config as it stood when this process first looked at the file.

let private configStalenessSource =
    """module TestData.ConfigStaleness

let logError (ex: exn) = printfn "logged: %A" ex

let riskyOperation () =
    try
        failwith "boom"
    with ex ->
        logError ex
"""

[<Fact>]
let ``an editorconfig edit changes the finding without restarting the process`` () =
    let dir = newConfiguredTree ()
    let file = Path.Combine(dir, "ConfigStaleness.fs")
    File.WriteAllText(file, configStalenessSource)

    let context =
        { getContextForSource configStalenessSource with
            FileName = file }

    // logError is the configured reporter, so the handler is compliant.
    writeEditorConfig dir "mga_error_reporting_functions = logError"

    let whileConfigured = errorReportingAnalyzer context |> Async.RunSynchronously

    test <@ whileConfigured.Length = 0 @>

    // The same handler, with logError no longer a reporter: it must now be flagged.
    writeEditorConfig dir "mga_error_reporting_functions = captureError"

    let afterConfigChange = errorReportingAnalyzer context |> Async.RunSynchronously

    test <@ afterConfigChange.Length = 1 @>
    test <@ afterConfigChange.[0].Code = "MGA-ERROR-REPORT-001" @>
