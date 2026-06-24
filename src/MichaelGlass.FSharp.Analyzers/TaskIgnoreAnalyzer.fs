/// <summary>
/// Flags <c>|> ignore</c> on Task and Async values that silently swallow exceptions.
/// An ignored Task's exceptions vanish — they won't crash the process or appear in logs.
/// Use <c>fireAndForget</c> for observed background work or <c>let! _ =</c> to explicitly discard.
/// </summary>
/// <remarks>
/// <para>Code: <c>MGA-TASK-IGNORE-001</c></para>
/// <para>Always enabled — no configuration needed.</para>
/// <para>Suppress with <c>// MGA-TASK-IGNORE-001:ok</c>.</para>
/// </remarks>
module MichaelGlass.FSharp.Analyzers.TaskIgnoreAnalyzer

open FSharp.Analyzers.SDK
open FSharp.Compiler.Symbols
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text

let private taskTypeNames =
    Set.ofList
        [ "System.Threading.Tasks.Task"
          "System.Threading.Tasks.Task`1"
          "System.Threading.Tasks.ValueTask"
          "System.Threading.Tasks.ValueTask`1"
          "Microsoft.FSharp.Control.FSharpAsync`1" ]

/// <summary>
/// Checks if a type (or any of its base types) is a Task or Async type.
/// </summary>
let private isTaskLikeType (fsharpType: FSharpType) : bool =
    try
        let rec checkType (t: FSharpType) =
            if t.HasTypeDefinition then
                let fullName =
                    match t.TypeDefinition.TryFullName with
                    | Some name -> Some name
                    | None ->
                        try
                            Some(t.TypeDefinition.AccessPath + "." + t.TypeDefinition.CompiledName)
                        with _ ->
                            None

                match fullName with
                | Some name when Set.contains name taskTypeNames -> true
                | _ ->
                    match t.BaseType with
                    | Some baseType -> checkType baseType
                    | None -> false
            else
                false

        checkType fsharpType
    with _ ->
        false

/// <summary>
/// Gets the return type of a function type (the last generic argument).
/// For <c>A -> B -> C</c>, returns <c>C</c>.
/// </summary>
let private getReturnType (fsharpType: FSharpType) : FSharpType option =
    try
        let rec loop (t: FSharpType) =
            if t.IsFunctionType && t.GenericArguments.Count >= 2 then
                loop t.GenericArguments.[t.GenericArguments.Count - 1]
            else
                Some t

        if fsharpType.IsFunctionType then
            loop fsharpType
        else
            Some fsharpType
    with _ ->
        None

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

/// <summary>
/// Returns the range of the expression that <em>produces the result value</em> of
/// <paramref name="expr"/> — i.e. the head of the outermost (last-applied) function.
/// </summary>
/// <remarks>
/// The ignored value's type is decided by the last function in the pipeline, not by
/// any sub-expression. For <c>a |&gt; Async.RunSynchronously</c> the result is produced
/// by <c>RunSynchronously</c> (an unwrapped, synchronous value) — even though the inner
/// <c>a</c> is an <c>Async</c>. Scanning every symbol in the range would wrongly treat
/// the whole expression as Task-like because of that inner <c>a</c>.
/// </remarks>
let rec private resultHeadRange (expr: SynExpr) : range =
    match expr with
    | SynExpr.Paren(expr = inner) -> resultHeadRange inner
    | SynExpr.Typed(expr = inner) -> resultHeadRange inner
    // `arg |> func` desugars to `App(App(op_PipeRight, arg), func)`: the result is
    // `func`'s return value, so the head is `func`.
    | SynExpr.App(funcExpr = SynExpr.App(funcExpr = op); argExpr = pipedFunc) when isPipeRight op ->
        resultHeadRange pipedFunc
    // `func arg`: the result is `func`'s return value, so the head is `func`.
    | SynExpr.App(funcExpr = funcExpr) -> resultHeadRange funcExpr
    | _ -> expr.Range

/// <summary>
/// Checks whether the value produced by the ignored expression is a Task/Async.
/// Looks up only the result-producing function (the last one applied), so an inner
/// Task/Async that has already been awaited/run (e.g. via <c>Async.RunSynchronously</c>)
/// does not cause a false positive.
/// </summary>
let private isIgnoredExprTaskLike (context: CliContext) (ignoredExpr: SynExpr) : bool =
    try
        let headRange = resultHeadRange ignoredExpr

        context.GetAllSymbolUsesOfFile()
        |> Seq.exists (fun symbolUse ->
            let r = symbolUse.Range

            r.StartLine = headRange.StartLine
            && r.EndLine = headRange.EndLine
            && r.StartColumn = headRange.StartColumn
            && r.EndColumn = headRange.EndColumn
            && (match symbolUse.Symbol with
                | :? FSharpMemberOrFunctionOrValue as mfv when mfv.FullType.IsFunctionType ->
                    match getReturnType mfv.FullType with
                    | Some retType -> isTaskLikeType retType
                    | None -> false
                | :? FSharpMemberOrFunctionOrValue as mfv -> isTaskLikeType mfv.FullType
                | _ -> false))
    with _ ->
        false

[<NoComparison; NoEquality>]
type private IgnoreUsage =
    { FullRange: range
      IgnoredExpr: SynExpr }

/// <summary>
/// CLI analyzer entry point. Walks AST for <c>ignore</c> calls and uses typed
/// check results to flag those applied to Task/Async expressions.
/// </summary>
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
                              IgnoredExpr = innerExpr }
                        )

                        false
                    // Pattern: ignore expr (direct call)
                    | SynExpr.App(funcExpr = funcExpr; argExpr = argExpr) when isIgnoreIdent funcExpr ->
                        usages.Add(
                            { FullRange = expr.Range
                              IgnoredExpr = argExpr }
                        )

                        false
                    | _ -> true)
                context.ParseFileResults.ParseTree

            let messages =
                usages
                |> Seq.toList
                |> List.filter (fun usage ->
                    isIgnoredExprTaskLike context usage.IgnoredExpr
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
