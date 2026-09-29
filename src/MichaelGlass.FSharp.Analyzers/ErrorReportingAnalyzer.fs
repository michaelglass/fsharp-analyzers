/// <summary>
/// Flags try/with blocks where catch handlers don't call a configured error-reporting
/// function. Silent catches hide production failures — this ensures every exception
/// handler reports to your observability stack.
/// </summary>
/// <remarks>
/// <para>Code: <c>MGA-ERROR-REPORT-001</c></para>
/// <para>Opt-in: set <c>mga_error_reporting_functions</c> in .editorconfig (e.g. <c>captureError, logError</c>).</para>
/// <para>Suppress with <c>// MGA-ERROR-REPORT-001:ok</c>.</para>
/// </remarks>
module MichaelGlass.FSharp.Analyzers.ErrorReportingAnalyzer

open FSharp.Analyzers.SDK
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text

let private getRequiredFunctions (fileName: string) =
    EditorConfig.getListProperty fileName "mga_error_reporting_functions"
    |> Set.ofList

/// <summary>
/// True when <paramref name="expr"/> names one of the required error-reporting functions,
/// bare (<c>logError</c>), qualified (<c>Log.logError</c>) or as a member
/// (<c>logger.captureError</c>, <c>(getLogger ()).captureError</c>).
/// </summary>
let private isReportingReference (requiredFunctions: Set<string>) (expr: SynExpr) : bool =
    let isRequired (id: Ident) =
        Set.contains id.idText requiredFunctions

    match expr with
    | SynExpr.Ident id -> isRequired id
    | SynExpr.LongIdent(longDotId = SynLongIdent(id = ids))
    | SynExpr.DotGet(longDotId = SynLongIdent(id = ids)) -> List.exists isRequired ids
    | _ -> false

/// <summary>
/// Core analysis logic, exposed for direct testing without editorconfig.
/// </summary>
/// <param name="requiredFunctions">Set of function names that must appear in catch handlers.</param>
/// <param name="context">The CLI analyzer context.</param>
/// <returns>List of warning messages for non-compliant try/with blocks.</returns>
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
                            not (AstWalk.existsExpr (isReportingReference requiredFunctions) handlerBody))
                    then
                        ranges.Add(tryWithRange)

                    true
                | _ -> true)
            context.ParseFileResults.ParseTree

        ranges
        |> Seq.toList
        |> List.filter (fun range -> not (Suppression.isLineSuppressed context.SourceText range "MGA-ERROR-REPORT-001"))
        |> List.map (fun range ->
            {
                Type = "Missing error reporting"
                Message =
                    "try/with block does not call a required error-reporting function. Add one of the configured mga_error_reporting_functions, or add '// MGA-ERROR-REPORT-001:ok' to suppress."
                Code = "MGA-ERROR-REPORT-001"
                Severity = Severity.Warning
                Range = range
                Fixes = []
            })

/// <summary>
/// CLI analyzer entry point. Reads required functions from editorconfig,
/// then delegates to <see cref="analyze"/>.
/// </summary>
[<CliAnalyzer("ErrorReportingAnalyzer",
              "Flags try/with blocks that don't call a configured error-reporting function (opt-in via editorconfig).")>]
let errorReportingAnalyzer: Analyzer<CliContext> =
    fun (context: CliContext) ->
        async {
            let requiredFunctions = getRequiredFunctions context.FileName
            return analyze requiredFunctions context
        }
