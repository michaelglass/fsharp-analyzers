module MichaelGlass.FSharp.Analyzers.Tests.TaskIgnoreAnalyzerTests

open Xunit
open Swensen.Unquote
open FSharp.Analyzers.SDK
open MichaelGlass.FSharp.Analyzers.Tests.Common
open MichaelGlass.FSharp.Analyzers.TaskIgnoreAnalyzer

[<Fact>]
let ``flags ignore on Task value`` () =
    let source = readTestData [ "task-ignore"; "IgnoredTask.fs" ]
    let context = getContextForSource source
    let messages = taskIgnoreAnalyzer context |> Async.RunSynchronously

    test <@ messages.Length = 1 @>
    test <@ messages.[0].Code = "MGA-TASK-IGNORE-001" @>
    test <@ messages.[0].Severity = Severity.Warning @>

[<Fact>]
let ``does not flag ignore on non-Task value`` () =
    let source = readTestData [ "task-ignore"; "IgnoredNonTask.fs" ]
    let context = getContextForSource source
    let messages = taskIgnoreAnalyzer context |> Async.RunSynchronously

    test <@ messages.Length = 0 @>

[<Fact>]
let ``does not flag suppressed Task ignore`` () =
    let source = readTestData [ "task-ignore"; "IgnoredTaskSuppressed.fs" ]
    let context = getContextForSource source
    let messages = taskIgnoreAnalyzer context |> Async.RunSynchronously

    test <@ messages.Length = 0 @>

[<Fact>]
let ``flags ignore on Async value`` () =
    let source = readTestData [ "task-ignore"; "IgnoredAsync.fs" ]
    let context = getContextForSource source
    let messages = taskIgnoreAnalyzer context |> Async.RunSynchronously

    test <@ messages.Length = 1 @>
    test <@ messages.[0].Code = "MGA-TASK-IGNORE-001" @>

[<Fact>]
let ``does not flag ignore on the synchronous result of Async.RunSynchronously`` () =
    let source = readTestData [ "task-ignore"; "IgnoredRunSyncResult.fs" ]
    let context = getContextForSource source
    let messages = taskIgnoreAnalyzer context |> Async.RunSynchronously

    test <@ messages.Length = 0 @>

[<Fact>]
let ``flags every ignore shape that discards a Task, ValueTask or Async, and nothing else`` () =
    let source = readTestData [ "task-ignore"; "IgnoreShapes.fs" ]
    let context = getContextForSource source
    let messages = taskIgnoreAnalyzer context |> Async.RunSynchronously

    let lines = source.Split('\n')

    let expected =
        lines
        |> Array.indexed
        |> Array.choose (fun (i, l) -> if l.EndsWith("// flag") then Some(i + 1) else None)
        |> Array.toList

    let flagged = messages |> List.map (fun m -> m.Range.StartLine) |> List.sort

    test <@ expected.Length = 7 @>
    test <@ flagged = expected @>
