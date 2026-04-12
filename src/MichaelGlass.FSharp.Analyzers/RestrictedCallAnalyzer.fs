/// Analyzer that flags restricted function calls based on editorconfig configuration.
/// Code: MGA-UNSAFE-CALL-001
module MichaelGlass.FSharp.Analyzers.RestrictedCallAnalyzer

open FSharp.Analyzers.SDK
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text

[<Literal>]
let Code = "MGA-UNSAFE-CALL-001"

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

/// Supports suffix matching for qualified names.
/// e.g. "Task.WhenAll" matches both "Task.WhenAll" and "System.Threading.Tasks.Task.WhenAll"
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
type private Diagnostic =
    { Range: range; Message: string }

/// Analyze a context with the given config. Exposed for testing.
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
                            |> Map.tryFindKey (fun pattern _ -> matchesFuncName pattern name)
                        with
                        | Some key ->
                            let expectedValue = config.BannedCallPatterns.[key]

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

[<CliAnalyzer("RestrictedCallAnalyzer",
              "Flags restricted function calls based on editorconfig configuration (opt-in).")>]
let restrictedCallAnalyzer: Analyzer<CliContext> =
    fun (context: CliContext) ->
        async {
            let config = loadConfig context.FileName
            return analyze config context
        }
