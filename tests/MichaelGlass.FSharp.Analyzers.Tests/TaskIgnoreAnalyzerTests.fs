module MichaelGlass.FSharp.Analyzers.Tests.TaskIgnoreAnalyzerTests

open Xunit
open Swensen.Unquote
open FSharp.Analyzers.SDK
open MichaelGlass.FSharp.Analyzers.Tests.Common
open MichaelGlass.FSharp.Analyzers.TaskIgnoreAnalyzer

[<Fact>]
let ``flags ignore on Task value`` () =
    let source =
        System.IO.File.ReadAllText(
            System.IO.Path.Combine(__SOURCE_DIRECTORY__, "data", "task-ignore", "IgnoredTask.fs")
        )

    let context = getContextForSource source
    let messages = taskIgnoreAnalyzer context |> Async.RunSynchronously

    test <@ messages.Length = 1 @>
    test <@ messages.[0].Code = "MGA-TASK-IGNORE-001" @>
    test <@ messages.[0].Severity = Severity.Warning @>

[<Fact>]
let ``does not flag ignore on non-Task value`` () =
    let source =
        System.IO.File.ReadAllText(
            System.IO.Path.Combine(__SOURCE_DIRECTORY__, "data", "task-ignore", "IgnoredNonTask.fs")
        )

    let context = getContextForSource source
    let messages = taskIgnoreAnalyzer context |> Async.RunSynchronously

    test <@ messages.Length = 0 @>

[<Fact>]
let ``does not flag suppressed Task ignore`` () =
    let source =
        System.IO.File.ReadAllText(
            System.IO.Path.Combine(__SOURCE_DIRECTORY__, "data", "task-ignore", "IgnoredTaskSuppressed.fs")
        )

    let context = getContextForSource source
    let messages = taskIgnoreAnalyzer context |> Async.RunSynchronously

    test <@ messages.Length = 0 @>
