/// Analyzer that flags try/with blocks where catch handlers don't call
/// a configured error-reporting function.
/// Opt-in: reads mga_error_reporting_functions from .editorconfig.
/// Code: MGA-ERROR-REPORT-001
module MichaelGlass.FSharp.Analyzers.ErrorReportingAnalyzer

open FSharp.Analyzers.SDK
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text

let private getRequiredFunctions (fileName: string) =
    EditorConfig.getListProperty fileName "mga_error_reporting_functions"
    |> Set.ofList

let private isRequiredCall (requiredFunctions: Set<string>) (name: string) =
    Set.contains name requiredFunctions

/// Recursively check if an expression contains a call to any required function.
/// This is a targeted search within an expression, separate from the tree walker.
let rec private containsRequiredCall (requiredFunctions: Set<string>) (expr: SynExpr) : bool =
    let recurse = containsRequiredCall requiredFunctions
    let isRequired = isRequiredCall requiredFunctions

    match expr with
    | SynExpr.Ident id -> isRequired id.idText
    | SynExpr.LongIdent(longDotId = SynLongIdent(id = ids)) -> ids |> List.exists (fun id -> isRequired id.idText)
    | SynExpr.DotGet(expr = inner; longDotId = SynLongIdent(id = ids)) ->
        ids |> List.exists (fun id -> isRequired id.idText)
        || recurse inner
    | SynExpr.App(funcExpr = func; argExpr = arg) -> recurse func || recurse arg
    | SynExpr.Paren(expr = inner) -> recurse inner
    | SynExpr.Typed(expr = inner) -> recurse inner
    | SynExpr.Sequential(expr1 = e1; expr2 = e2) -> recurse e1 || recurse e2
    | SynExpr.LetOrUse(bindings = bindings; body = body) ->
        bindings |> List.exists (fun (SynBinding(expr = e)) -> recurse e)
        || recurse body
    | SynExpr.IfThenElse(ifExpr = cond; thenExpr = thenExpr; elseExpr = elseExprOpt) ->
        recurse cond
        || recurse thenExpr
        || (match elseExprOpt with
            | Some e -> recurse e
            | None -> false)
    | SynExpr.Match(clauses = clauses)
    | SynExpr.MatchLambda(matchClauses = clauses)
    | SynExpr.MatchBang(clauses = clauses) ->
        clauses
        |> List.exists (fun (SynMatchClause(resultExpr = body)) -> recurse body)
    | SynExpr.TryWith(tryExpr = body; withCases = clauses) ->
        recurse body
        || clauses
           |> List.exists (fun (SynMatchClause(resultExpr = body)) -> recurse body)
    | SynExpr.TryFinally(tryExpr = body; finallyExpr = fin) -> recurse body || recurse fin
    | SynExpr.ComputationExpr(expr = expr) -> recurse expr
    | SynExpr.Lambda(body = body) -> recurse body
    | SynExpr.DoBang(expr = expr) -> recurse expr
    | SynExpr.YieldOrReturn(expr = expr) -> recurse expr
    | SynExpr.YieldOrReturnFrom(expr = expr) -> recurse expr
    | SynExpr.ForEach(bodyExpr = body) -> recurse body
    | SynExpr.Tuple(exprs = exprs) -> exprs |> List.exists recurse
    | SynExpr.ArrayOrList(exprs = exprs) -> exprs |> List.exists recurse
    | SynExpr.Record(copyInfo = copyExprOpt; recordFields = fields) ->
        (match copyExprOpt with
         | Some(e, _) -> recurse e
         | None -> false)
        || fields
           |> List.exists (fun (SynExprRecordField(expr = exprOpt)) ->
               match exprOpt with
               | Some e -> recurse e
               | None -> false)
    | _ -> false

/// Core analysis logic, exposed for testing.
/// Takes requiredFunctions explicitly so tests can provide them without editorconfig.
let analyze (requiredFunctions: Set<string>) (context: CliContext) : Message list =
    if Set.isEmpty requiredFunctions then
        []
    else
        let ranges = ResizeArray<range>()

        AstWalk.walkParseTree
            (fun expr ->
                match expr with
                | SynExpr.TryWith(withCases = clauses; range = tryWithRange) ->
                    if
                        clauses
                        |> List.exists (fun (SynMatchClause(resultExpr = handlerBody)) ->
                            not (containsRequiredCall requiredFunctions handlerBody))
                    then
                        ranges.Add(tryWithRange)

                    true
                | _ -> true)
            context.ParseFileResults.ParseTree

        ranges
        |> Seq.toList
        |> List.filter (fun range ->
            not (Suppression.isLineSuppressed context.SourceText range "MGA-ERROR-REPORT-001"))
        |> List.map (fun range ->
            { Type = "Missing error reporting"
              Message =
                "try/with block does not call a required error-reporting function. Add one of the configured mga_error_reporting_functions, or add '// MGA-ERROR-REPORT-001:ok' to suppress."
              Code = "MGA-ERROR-REPORT-001"
              Severity = Severity.Warning
              Range = range
              Fixes = [] })

[<CliAnalyzer("ErrorReportingAnalyzer",
              "Flags try/with blocks that don't call a configured error-reporting function (opt-in via editorconfig).")>]
let errorReportingAnalyzer: Analyzer<CliContext> =
    fun (context: CliContext) ->
        async {
            let requiredFunctions = getRequiredFunctions context.FileName
            return analyze requiredFunctions context
        }
