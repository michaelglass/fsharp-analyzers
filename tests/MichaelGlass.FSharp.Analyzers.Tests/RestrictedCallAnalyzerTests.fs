module MichaelGlass.FSharp.Analyzers.Tests.RestrictedCallAnalyzerTests

open Xunit
open Swensen.Unquote
open FSharp.Analyzers.SDK
open MichaelGlass.FSharp.Analyzers.Tests.Common
open MichaelGlass.FSharp.Analyzers.RestrictedCallAnalyzer

let private configWithAll =
    {
        BannedFunctions = Set.ofList [ "Task.WhenAll"; "Thread.Sleep" ]
        BannedCallPatterns = Map.ofList [ "Attr.type'", "submit" ]
        UnsafeDynamicArgFunctions = Set.ofList [ "Text.raw" ]
    }

[<Fact>]
let ``flags banned function`` () =
    let source = readTestData [ "restricted-call"; "BannedFunction.fs" ]
    let context = getContextForSource source
    let messages = analyze configWithAll context

    test <@ messages.Length = 1 @>
    test <@ messages.[0].Code = "MGA-UNSAFE-CALL-001" @>
    test <@ messages.[0].Severity = Severity.Warning @>

[<Fact>]
let ``flags banned function via pipe`` () =
    let source = readTestData [ "restricted-call"; "BannedFunctionPiped.fs" ]
    let context = getContextForSource source
    let messages = analyze configWithAll context

    test <@ messages.Length = 1 @>
    test <@ messages.[0].Code = "MGA-UNSAFE-CALL-001" @>
    test <@ messages.[0].Severity = Severity.Warning @>

[<Fact>]
let ``flags banned call pattern`` () =
    let source = readTestData [ "restricted-call"; "BannedCallPattern.fs" ]
    let context = getContextForSource source
    let messages = analyze configWithAll context

    test <@ messages.Length = 1 @>
    test <@ messages.[0].Code = "MGA-UNSAFE-CALL-001" @>
    test <@ messages.[0].Severity = Severity.Warning @>

[<Fact>]
let ``flags unsafe dynamic arg`` () =
    let source = readTestData [ "restricted-call"; "UnsafeDynamicArg.fs" ]
    let context = getContextForSource source
    let messages = analyze configWithAll context

    test <@ messages.Length = 1 @>
    test <@ messages.[0].Code = "MGA-UNSAFE-CALL-001" @>
    test <@ messages.[0].Severity = Severity.Warning @>

[<Fact>]
let ``does not flag safe static arg`` () =
    let source = readTestData [ "restricted-call"; "SafeStaticArg.fs" ]
    let context = getContextForSource source
    let messages = analyze configWithAll context

    test <@ messages.Length = 0 @>

[<Fact>]
let ``returns empty with no config`` () =
    let source = readTestData [ "restricted-call"; "no-config"; "NoConfig.fs" ]

    let emptyConfig =
        {
            BannedFunctions = Set.empty
            BannedCallPatterns = Map.empty
            UnsafeDynamicArgFunctions = Set.empty
        }

    let context = getContextForSource source
    let messages = analyze emptyConfig context

    test <@ messages.Length = 0 @>

// --- The CLI entry point, configured from .editorconfig ------------------------------
// The tests above hand `analyze` a Config directly. A host only ever calls the
// [<CliAnalyzer>] entry point, which builds that Config from the analysed file's
// .editorconfig; these run the same fixtures through that path.

let private runFromEditorConfig (properties: string) (fixture: string) =
    let dir = newConfiguredTree ()
    writeEditorConfig dir properties
    let source = readTestData [ "restricted-call"; fixture ]

    let context =
        { getContextForSource source with
            FileName = System.IO.Path.Combine(dir, fixture)
        }

    restrictedCallAnalyzer context |> Async.RunSynchronously

let private allThreeChecks =
    "mga_banned_functions = Task.WhenAll, Thread.Sleep\n"
    + "mga_banned_call_patterns = Attr.type':submit\n"
    + "mga_unsafe_dynamic_arg_functions = Text.raw"

[<Theory>]
[<InlineData("BannedFunction.fs")>]
[<InlineData("BannedCallPattern.fs")>]
[<InlineData("UnsafeDynamicArg.fs")>]
let ``the entry point reads each check from editorconfig`` (fixture: string) =
    let messages = runFromEditorConfig allThreeChecks fixture

    test <@ messages |> List.map _.Code = [ "MGA-UNSAFE-CALL-001" ] @>

[<Fact>]
let ``the entry point reports nothing when editorconfig configures no check`` () =
    let messages = runFromEditorConfig "unrelated_key = 1" "BannedFunction.fs"

    test <@ List.isEmpty messages @>

[<Fact>]
let ``a call pattern without a colon is skipped, not read as a pattern`` () =
    // `name:value` is the only shape. The malformed entry comes AFTER a valid one for the
    // same function: were it read as a pattern (with any value), it would replace the
    // valid entry in the map and the fixture's `submit` call would go unreported.
    let messages =
        runFromEditorConfig "mga_banned_call_patterns = Attr.type':submit, Attr.type'" "BannedCallPattern.fs"

    test <@ messages |> List.map _.Code = [ "MGA-UNSAFE-CALL-001" ] @>
