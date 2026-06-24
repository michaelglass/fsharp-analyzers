module MichaelGlass.FSharp.Analyzers.Tests.RawSqlAnalyzerTests

open Xunit
open Swensen.Unquote
open FSharp.Analyzers.SDK
open MichaelGlass.FSharp.Analyzers.Tests.Common
open MichaelGlass.FSharp.Analyzers.RawSqlAnalyzer

[<Fact>]
let ``flags raw SQL string`` () =
    let source = readTestData [ "rawsql"; "RawSqlString.fs" ]
    let context = getContextForSource source
    let messages = rawSqlAnalyzer context |> Async.RunSynchronously

    test <@ messages.Length = 1 @>
    test <@ messages.[0].Code = "MGA-RAWSQL-001" @>
    test <@ messages.[0].Severity = Severity.Warning @>

[<Fact>]
let ``flags a short upper-cased SQL statement without a FROM clause`` () =
    let source = readTestData [ "rawsql"; "UppercaseSelectOne.fs" ]
    let context = getContextForSource source
    let messages = rawSqlAnalyzer context |> Async.RunSynchronously

    test <@ messages.Length = 1 @>
    test <@ messages.[0].Code = "MGA-RAWSQL-001" @>

[<Fact>]
let ``does not flag non-SQL string`` () =
    let source = readTestData [ "rawsql"; "NonSqlString.fs" ]
    let context = getContextForSource source
    let messages = rawSqlAnalyzer context |> Async.RunSynchronously

    test <@ messages.Length = 0 @>

[<Fact>]
let ``does not flag suppressed SQL string`` () =
    let source = readTestData [ "rawsql"; "RawSqlSuppressed.fs" ]
    let context = getContextForSource source
    let messages = rawSqlAnalyzer context |> Async.RunSynchronously

    test <@ messages.Length = 0 @>

[<Fact>]
let ``does not flag English prose that merely starts with a SQL keyword`` () =
    let source = readTestData [ "rawsql"; "EnglishPrefixedByKeyword.fs" ]
    let context = getContextForSource source
    let messages = rawSqlAnalyzer context |> Async.RunSynchronously

    test <@ messages.Length = 0 @>

[<Fact>]
let ``does not flag a bare upper-cased keyword label`` () =
    let source = readTestData [ "rawsql"; "BareKeywordLabel.fs" ]
    let context = getContextForSource source
    let messages = rawSqlAnalyzer context |> Async.RunSynchronously

    test <@ messages.Length = 0 @>
