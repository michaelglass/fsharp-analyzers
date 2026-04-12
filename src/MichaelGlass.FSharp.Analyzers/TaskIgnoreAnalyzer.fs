/// Analyzer that flags `|> ignore` on Task/Async values that silently swallow exceptions.
/// Use `fireAndForget` for observed background tasks or `let! _ =` to explicitly discard.
/// Code: MGA-TASK-IGNORE-001
module MichaelGlass.FSharp.Analyzers.TaskIgnoreAnalyzer

open FSharp.Analyzers.SDK
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text

let private taskPatterns =
    [| "Task.Run"
       "Task.Factory"
       "Task.Delay"
       "ContinueWith"
       ":> Task"
       ":> System.Threading.Tasks.Task"
       "Async.StartAsTask"
       "Async.Start("
       "startAsTask"
       "StartAsTask"
       "task {"
       "async {" |]

let private getSourceText (sourceText: ISourceText) (range: range) : string =
    try
        let startLine = range.StartLine - 1
        let endLine = range.EndLine - 1
        let lineCount = sourceText.GetLineCount()
        let lines = ResizeArray<string>()

        for i in max 0 startLine .. min endLine (lineCount - 1) do
            lines.Add(sourceText.GetLineString(i))

        System.String.Join("\n", lines)
    with _ ->
        ""

let private containsTaskPattern (text: string) : bool =
    taskPatterns |> Array.exists text.Contains

let private isIgnoreIdent (expr: SynExpr) =
    match expr with
    | SynExpr.Ident id when id.idText = "ignore" -> true
    | SynExpr.LongIdent(longDotId = SynLongIdent(id = ids)) ->
        match ids with
        | [] -> false
        | _ ->
            let last = List.last ids
            last.idText = "ignore"
    | _ -> false

let private isPipeRight (expr: SynExpr) =
    match expr with
    | SynExpr.Ident id when id.idText = "op_PipeRight" -> true
    | SynExpr.LongIdent(longDotId = SynLongIdent(id = ids)) ->
        match ids with
        | [] -> false
        | _ ->
            let last = List.last ids
            last.idText = "op_PipeRight"
    | _ -> false

[<NoComparison>]
type private IgnoreUsage =
    { FullRange: range
      IgnoredRange: range }

[<CliAnalyzer("TaskIgnoreAnalyzer",
              "Flags |> ignore on Task/Async values that silently swallow exceptions. Use fireAndForget or let! _ = instead.")>]
let taskIgnoreAnalyzer: Analyzer<CliContext> =
    fun (context: CliContext) ->
        async {
            let usages = ResizeArray<IgnoreUsage>()

            AstWalk.walkParseTree
                (fun expr ->
                    match expr with
                    // Pattern: expr |> ignore
                    | SynExpr.App(funcExpr = SynExpr.App(funcExpr = op; argExpr = innerExpr); argExpr = ignoreExpr) when
                        isPipeRight op && isIgnoreIdent ignoreExpr
                        ->
                        usages.Add(
                            { FullRange = expr.Range
                              IgnoredRange = innerExpr.Range }
                        )

                        false // Don't recurse into the ignored sub-expression
                    // Pattern: ignore expr (direct call)
                    | SynExpr.App(funcExpr = funcExpr; argExpr = argExpr) when isIgnoreIdent funcExpr ->
                        usages.Add(
                            { FullRange = expr.Range
                              IgnoredRange = argExpr.Range }
                        )

                        false
                    | _ -> true)
                context.ParseFileResults.ParseTree

            let messages =
                usages
                |> Seq.toList
                |> List.filter (fun usage ->
                    let text = getSourceText context.SourceText usage.IgnoredRange
                    containsTaskPattern text
                    && not (Suppression.isLineSuppressed context.SourceText usage.FullRange "MGA-TASK-IGNORE-001"))
                |> List.map (fun usage ->
                    { Type = "Task |> ignore"
                      Message =
                        "Ignoring a Task/Async silently swallows exceptions. Use `fireAndForget` for background tasks or `let! _ =` to explicitly discard. Add '// MGA-TASK-IGNORE-001:ok' to suppress."
                      Code = "MGA-TASK-IGNORE-001"
                      Severity = Severity.Warning
                      Range = usage.FullRange
                      Fixes = [] })

            return messages
        }
