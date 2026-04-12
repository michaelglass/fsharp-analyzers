module MichaelGlass.FSharp.Analyzers.Tests.TaskIgnoreAnalyzerTests

open Xunit
open Swensen.Unquote
open FSharp.Analyzers.SDK
open FSharp.Analyzers.SDK.Testing
open MichaelGlass.FSharp.Analyzers.TaskIgnoreAnalyzer

let private projectOptions =
    mkOptionsFromProject "net10.0" []
    |> Async.AwaitTask
    |> Async.RunSynchronously

let private getContextForSource (source: string) =
    getContext projectOptions source

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
