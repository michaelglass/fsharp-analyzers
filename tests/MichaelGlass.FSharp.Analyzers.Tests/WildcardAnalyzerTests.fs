module MichaelGlass.FSharp.Analyzers.Tests.WildcardAnalyzerTests

open Xunit
open Swensen.Unquote
open FSharp.Analyzers.SDK
open MichaelGlass.FSharp.Analyzers.Tests.Common
open MichaelGlass.FSharp.Analyzers.WildcardAnalyzer

[<Fact>]
let ``flags wildcard on custom DU`` () =
    let source = readTestData [ "wildcard"; "WildcardOnDU.fs" ]
    let context = getContextForSource source
    let messages = wildcardAnalyzer context |> Async.RunSynchronously

    test <@ messages.Length = 1 @>
    test <@ messages.[0].Code = "MGA-WILDCARD-001" @>
    test <@ messages.[0].Severity = Severity.Warning @>

[<Fact>]
let ``does not flag wildcard on option type`` () =
    let source = readTestData [ "wildcard"; "WildcardOnOption.fs" ]
    let context = getContextForSource source
    let messages = wildcardAnalyzer context |> Async.RunSynchronously

    test <@ messages.Length = 0 @>

[<Fact>]
let ``does not flag suppressed wildcard`` () =
    let source = readTestData [ "wildcard"; "WildcardSuppressed.fs" ]
    let context = getContextForSource source
    let messages = wildcardAnalyzer context |> Async.RunSynchronously

    test <@ messages.Length = 0 @>
