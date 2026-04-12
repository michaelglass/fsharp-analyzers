/// Analyzer that flags `|> ignore` on Task/Async values that silently swallow exceptions.
/// Use `fireAndForget` for observed background tasks or `let! _ =` to explicitly discard.
/// Code: MGA-TASK-IGNORE-001
module MichaelGlass.FSharp.Analyzers.TaskIgnoreAnalyzer

open FSharp.Analyzers.SDK
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text

/// Source-text patterns that suggest a Task or Async is being created/cast
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

/// Extract source text covering a range (inclusive of all lines)
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

/// Check if source text contains any Task-related pattern
let private containsTaskPattern (text: string) : bool =
    taskPatterns |> Array.exists text.Contains

/// Check if expr is the `ignore` identifier
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

/// Check if expr is the pipe-right operator
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

/// Ranges of `|> ignore` or `ignore(expr)` where the ignored expression looks Task-related
[<NoComparison>]
type private IgnoreUsage =
    { FullRange: range
      IgnoredRange: range }

/// Walk expressions looking for ignore patterns. When a match is found,
/// we do NOT recurse into the ignored sub-expression (avoids double-flagging).
let rec private walkExpr (usages: ResizeArray<IgnoreUsage>) (expr: SynExpr) =
    match expr with
    // Pattern: expr |> ignore
    // AST: App(NonAtomic, false, App(NonAtomic, true, op_PipeRight, expr, _), ignore, _)
    | SynExpr.App(funcExpr = SynExpr.App(funcExpr = op; argExpr = innerExpr); argExpr = ignoreExpr) when
        isPipeRight op && isIgnoreIdent ignoreExpr
        ->
        usages.Add(
            { FullRange = expr.Range
              IgnoredRange = innerExpr.Range }
        )
    // Don't recurse — we captured the ignore usage

    // Pattern: ignore expr (direct call)
    | SynExpr.App(funcExpr = funcExpr; argExpr = argExpr) when isIgnoreIdent funcExpr ->
        usages.Add(
            { FullRange = expr.Range
              IgnoredRange = argExpr.Range }
        )

    // Everything else: recurse normally
    | SynExpr.App(funcExpr = func; argExpr = arg) ->
        walkExpr usages func
        walkExpr usages arg
    | SynExpr.Paren(expr = inner) -> walkExpr usages inner
    | SynExpr.Typed(expr = inner) -> walkExpr usages inner
    | SynExpr.Tuple(exprs = exprs) ->
        for e in exprs do
            walkExpr usages e
    | SynExpr.ArrayOrList(exprs = exprs) ->
        for e in exprs do
            walkExpr usages e
    | SynExpr.Record(copyInfo = copyExprOpt; recordFields = fields) ->
        match copyExprOpt with
        | Some(e, _) -> walkExpr usages e
        | None -> ()

        for field in fields do
            match field with
            | SynExprRecordField(expr = exprOpt) ->
                match exprOpt with
                | Some e -> walkExpr usages e
                | None -> ()
    | SynExpr.New(expr = expr) -> walkExpr usages expr
    | SynExpr.ObjExpr(argOptions = argOpt; bindings = bindings) ->
        match argOpt with
        | Some(e, _) -> walkExpr usages e
        | None -> ()

        for binding in bindings do
            walkBinding usages binding
    | SynExpr.While(whileExpr = cond; doExpr = body) ->
        walkExpr usages cond
        walkExpr usages body
    | SynExpr.For(identBody = start; toBody = finish; doBody = body) ->
        walkExpr usages start
        walkExpr usages finish
        walkExpr usages body
    | SynExpr.ForEach(enumExpr = enumExpr; bodyExpr = body) ->
        walkExpr usages enumExpr
        walkExpr usages body
    | SynExpr.ArrayOrListComputed(expr = expr) -> walkExpr usages expr
    | SynExpr.ComputationExpr(expr = expr) -> walkExpr usages expr
    | SynExpr.Lambda(body = body) -> walkExpr usages body
    | SynExpr.Assert(expr = expr) -> walkExpr usages expr
    | SynExpr.Match(expr = scrutinee; clauses = clauses)
    | SynExpr.MatchBang(expr = scrutinee; clauses = clauses) ->
        walkExpr usages scrutinee

        for clause in clauses do
            match clause with
            | SynMatchClause(resultExpr = body) -> walkExpr usages body
    | SynExpr.MatchLambda(matchClauses = clauses) ->
        for clause in clauses do
            match clause with
            | SynMatchClause(resultExpr = body) -> walkExpr usages body
    | SynExpr.LetOrUse(bindings = bindings; body = body) ->
        for binding in bindings do
            walkBinding usages binding

        walkExpr usages body
    | SynExpr.TryWith(tryExpr = body; withCases = clauses) ->
        walkExpr usages body

        for clause in clauses do
            match clause with
            | SynMatchClause(resultExpr = body) -> walkExpr usages body
    | SynExpr.TryFinally(tryExpr = body; finallyExpr = finallyExpr) ->
        walkExpr usages body
        walkExpr usages finallyExpr
    | SynExpr.Lazy(expr = expr) -> walkExpr usages expr
    | SynExpr.Sequential(expr1 = e1; expr2 = e2) ->
        walkExpr usages e1
        walkExpr usages e2
    | SynExpr.IfThenElse(ifExpr = cond; thenExpr = thenExpr; elseExpr = elseExprOpt) ->
        walkExpr usages cond
        walkExpr usages thenExpr

        match elseExprOpt with
        | Some e -> walkExpr usages e
        | None -> ()
    | SynExpr.LongIdentSet(expr = expr) -> walkExpr usages expr
    | SynExpr.DotGet(expr = expr) -> walkExpr usages expr
    | SynExpr.DotSet(targetExpr = target; rhsExpr = value) ->
        walkExpr usages target
        walkExpr usages value
    | SynExpr.Set(targetExpr = target; rhsExpr = value) ->
        walkExpr usages target
        walkExpr usages value
    | SynExpr.DotIndexedGet(objectExpr = expr; indexArgs = indexArgs) ->
        walkExpr usages expr
        walkExpr usages indexArgs
    | SynExpr.DotIndexedSet(objectExpr = target; indexArgs = indexArgs; valueExpr = value) ->
        walkExpr usages target
        walkExpr usages indexArgs
        walkExpr usages value
    | SynExpr.NamedIndexedPropertySet(expr1 = e1; expr2 = e2) ->
        walkExpr usages e1
        walkExpr usages e2
    | SynExpr.DotNamedIndexedPropertySet(targetExpr = target; argExpr = e1; rhsExpr = e2) ->
        walkExpr usages target
        walkExpr usages e1
        walkExpr usages e2
    | SynExpr.TypeTest(expr = expr) -> walkExpr usages expr
    | SynExpr.Upcast(expr = expr) -> walkExpr usages expr
    | SynExpr.Downcast(expr = expr) -> walkExpr usages expr
    | SynExpr.InferredUpcast(expr = expr) -> walkExpr usages expr
    | SynExpr.InferredDowncast(expr = expr) -> walkExpr usages expr
    | SynExpr.AddressOf(expr = expr) -> walkExpr usages expr
    | SynExpr.JoinIn(lhsExpr = e1; rhsExpr = e2) ->
        walkExpr usages e1
        walkExpr usages e2
    | SynExpr.YieldOrReturn(expr = expr) -> walkExpr usages expr
    | SynExpr.YieldOrReturnFrom(expr = expr) -> walkExpr usages expr
    | SynExpr.DoBang(expr = expr) -> walkExpr usages expr
    | SynExpr.TraitCall(argExpr = expr) -> walkExpr usages expr
    | SynExpr.IndexFromEnd(expr = expr) -> walkExpr usages expr
    | SynExpr.IndexRange(expr1 = e1Opt; expr2 = e2Opt) ->
        match e1Opt with
        | Some e -> walkExpr usages e
        | None -> ()

        match e2Opt with
        | Some e -> walkExpr usages e
        | None -> ()
    | SynExpr.DebugPoint(innerExpr = expr) -> walkExpr usages expr
    | SynExpr.InterpolatedString(contents = parts) ->
        for part in parts do
            match part with
            | SynInterpolatedStringPart.FillExpr(fillExpr = expr) -> walkExpr usages expr
            | SynInterpolatedStringPart.String _ -> ()
    | _ -> ()

and private walkBinding (usages: ResizeArray<IgnoreUsage>) (binding: SynBinding) =
    match binding with
    | SynBinding(expr = body) -> walkExpr usages body

let rec private walkModuleDecl (usages: ResizeArray<IgnoreUsage>) (decl: SynModuleDecl) =
    match decl with
    | SynModuleDecl.Let(bindings = bindings) ->
        for binding in bindings do
            walkBinding usages binding
    | SynModuleDecl.Expr(expr = expr) -> walkExpr usages expr
    | SynModuleDecl.NestedModule(decls = decls) ->
        for d in decls do
            walkModuleDecl usages d
    | SynModuleDecl.Types(typeDefns = typeDefs) ->
        for typeDef in typeDefs do
            walkTypeDef usages typeDef
    | _ -> ()

and private walkTypeDef (usages: ResizeArray<IgnoreUsage>) (typeDef: SynTypeDefn) =
    match typeDef with
    | SynTypeDefn(members = members) ->
        for mem in members do
            walkMemberDefn usages mem

and private walkMemberDefn (usages: ResizeArray<IgnoreUsage>) (mem: SynMemberDefn) =
    match mem with
    | SynMemberDefn.Member(memberDefn = binding) -> walkBinding usages binding
    | SynMemberDefn.LetBindings(bindings = bindings) ->
        for binding in bindings do
            walkBinding usages binding
    | SynMemberDefn.GetSetMember(memberDefnForGet = getBinding; memberDefnForSet = setBinding) ->
        match getBinding with
        | Some b -> walkBinding usages b
        | None -> ()

        match setBinding with
        | Some b -> walkBinding usages b
        | None -> ()
    | SynMemberDefn.AutoProperty(synExpr = expr) -> walkExpr usages expr
    | _ -> ()

/// Walk the entire parse tree and collect ignore usages
let private walkParseTree (parseTree: ParsedInput) : IgnoreUsage list =
    let usages = ResizeArray<IgnoreUsage>()

    match parseTree with
    | ParsedInput.ImplFile(ParsedImplFileInput(contents = modules)) ->
        for moduleOrNs in modules do
            match moduleOrNs with
            | SynModuleOrNamespace(decls = decls) ->
                for decl in decls do
                    walkModuleDecl usages decl
    | ParsedInput.SigFile _ -> ()

    usages |> Seq.toList

[<CliAnalyzer("TaskIgnoreAnalyzer",
              "Flags |> ignore on Task/Async values that silently swallow exceptions. Use fireAndForget or let! _ = instead.")>]
let taskIgnoreAnalyzer: Analyzer<CliContext> =
    fun (context: CliContext) ->
        async {
            let usages = walkParseTree context.ParseFileResults.ParseTree

            let messages =
                usages
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
