/// <summary>
/// Flags restricted function calls based on editorconfig configuration.
/// Three independent checks: banned functions, banned call patterns (function + argument
/// value), and unsafe dynamic arguments (non-literal values passed to injection-sensitive functions).
/// </summary>
/// <remarks>
/// <para>Code: <c>MGA-UNSAFE-CALL-001</c></para>
/// <para>Opt-in via .editorconfig: <c>mga_banned_functions</c>, <c>mga_banned_call_patterns</c>,
/// <c>mga_unsafe_dynamic_arg_functions</c>.</para>
/// <para>Suppress with <c>// MGA-UNSAFE-CALL-001:ok</c>.</para>
/// </remarks>
module MichaelGlass.FSharp.Analyzers.RestrictedCallAnalyzer

open FSharp.Analyzers.SDK
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text

[<Literal>]
let Code = "MGA-UNSAFE-CALL-001"

/// <summary>Configuration for the three restricted call checks.</summary>
type Config =
    { BannedFunctions: Set<string>
      BannedCallPatterns: Map<string, string>
      UnsafeDynamicArgFunctions: Set<string> }

let private loadConfig (fileName: string) : Config =
    let banned =
        EditorConfig.getListProperty fileName "mga_banned_functions" |> Set.ofList

    let patterns =
        EditorConfig.getListProperty fileName "mga_banned_call_patterns"
        |> List.choose (fun s ->
            match s.IndexOf(':') with
            | -1 -> None
            | i -> Some(s.Substring(0, i).Trim(), s.Substring(i + 1).Trim()))
        |> Map.ofList

    let unsafeDynamic =
        EditorConfig.getListProperty fileName "mga_unsafe_dynamic_arg_functions"
        |> Set.ofList

    { BannedFunctions = banned
      BannedCallPatterns = patterns
      UnsafeDynamicArgFunctions = unsafeDynamic }

let private isConfigEmpty (config: Config) =
    Set.isEmpty config.BannedFunctions
    && Map.isEmpty config.BannedCallPatterns
    && Set.isEmpty config.UnsafeDynamicArgFunctions

let private getFuncName (expr: SynExpr) : string option =
    match expr with
    | SynExpr.Ident id -> Some id.idText
    | SynExpr.LongIdent(longDotId = SynLongIdent(id = ids)) ->
        ids |> List.map (fun i -> i.idText) |> String.concat "." |> Some
    | _ -> None

/// <summary>
/// Supports suffix matching for qualified names.
/// E.g. "Task.WhenAll" matches both "Task.WhenAll" and "System.Threading.Tasks.Task.WhenAll".
/// </summary>
let private matchesFuncName (configuredName: string) (actualName: string) =
    actualName = configuredName || actualName.EndsWith("." + configuredName)

let private isStringLiteral (expr: SynExpr) =
    match expr with
    | SynExpr.Const(SynConst.String _, _) -> true
    | _ -> false

let private getStringValue (expr: SynExpr) =
    match expr with
    | SynExpr.Const(SynConst.String(text = text), _) -> Some text
    | _ -> None

[<NoComparison; NoEquality>]
type private Diagnostic = { Range: range; Message: string }

/// <summary>
/// Core analysis logic, exposed for direct testing without editorconfig.
/// </summary>
/// <param name="config">The restricted call configuration.</param>
/// <param name="context">The CLI analyzer context.</param>
/// <returns>List of warning messages for restricted calls found.</returns>
let analyze (config: Config) (context: CliContext) : Message list =
    if isConfigEmpty config then
        []
    else
        let diagnostics = ResizeArray<Diagnostic>()

        AstWalk.walkParseTree
            (fun expr ->
                // Check for banned function references (standalone idents, not just in App position)
                match expr with
                | SynExpr.Ident _
                | SynExpr.LongIdent _ ->
                    match getFuncName expr with
                    | Some name ->
                        if config.BannedFunctions |> Set.exists (fun banned -> matchesFuncName banned name) then
                            diagnostics.Add(
                                { Range = expr.Range
                                  Message =
                                    $"Banned function call '%s{name}' is not allowed. Add '// %s{Code}:ok' with a reason to suppress." }
                            )
                    | None -> ()
                | _ -> ()

                // Check SynExpr.App for call patterns and unsafe dynamic args
                match expr with
                | SynExpr.App(funcExpr = funcExpr; argExpr = argExpr) ->
                    match getFuncName funcExpr with
                    | Some name ->
                        match
                            config.BannedCallPatterns
                            |> Map.tryPick (fun pattern value ->
                                if matchesFuncName pattern name then Some value else None)
                        with
                        | Some expectedValue ->
                            match getStringValue argExpr with
                            | Some actualValue when actualValue = expectedValue ->
                                diagnostics.Add(
                                    { Range = expr.Range
                                      Message =
                                        $"Banned call pattern '%s{name} \"%s{expectedValue}\"' is not allowed. Add '// %s{Code}:ok' with a reason to suppress." }
                                )
                            | _ -> ()
                        | None -> ()

                        if
                            config.UnsafeDynamicArgFunctions
                            |> Set.exists (fun unsafeName -> matchesFuncName unsafeName name)
                            && not (isStringLiteral argExpr)
                        then
                            diagnostics.Add(
                                { Range = expr.Range
                                  Message =
                                    $"'%s{name}' called with a dynamic argument — only string literals are safe here. Add '// %s{Code}:ok' with a reason to suppress." }
                            )
                    | None -> ()
                | _ -> ()

                true)
            context.ParseFileResults.ParseTree

        diagnostics
        |> Seq.toList
        |> List.filter (fun d -> not (Suppression.isLineSuppressed context.SourceText d.Range Code))
        |> List.map (fun d ->
            { Type = "Restricted call"
              Message = d.Message
              Code = Code
              Severity = Severity.Warning
              Range = d.Range
              Fixes = [] })

/// <summary>
/// CLI analyzer entry point. Reads configuration from editorconfig,
/// then delegates to <see cref="analyze"/>.
/// </summary>
[<CliAnalyzer("RestrictedCallAnalyzer", "Flags restricted function calls based on editorconfig configuration (opt-in).")>]
let restrictedCallAnalyzer: Analyzer<CliContext> =
    fun (context: CliContext) ->
        async {
            let config = loadConfig context.FileName
            return analyze config context
        }
