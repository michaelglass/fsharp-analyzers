module MichaelGlass.FSharp.Analyzers.Tests.RestrictedCallAnalyzerTests

open Xunit
open Swensen.Unquote
open FSharp.Analyzers.SDK
open MichaelGlass.FSharp.Analyzers.Tests.Common
open MichaelGlass.FSharp.Analyzers.RestrictedCallAnalyzer

let private configWithAll =
    { BannedFunctions = Set.ofList [ "Task.WhenAll"; "Thread.Sleep" ]
      BannedCallPatterns = Map.ofList [ "Attr.type'", "submit" ]
      UnsafeDynamicArgFunctions = Set.ofList [ "Text.raw" ] }

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
        { BannedFunctions = Set.empty
          BannedCallPatterns = Map.empty
          UnsafeDynamicArgFunctions = Set.empty }

    let context = getContextForSource source
    let messages = analyze emptyConfig context

    test <@ messages.Length = 0 @>
