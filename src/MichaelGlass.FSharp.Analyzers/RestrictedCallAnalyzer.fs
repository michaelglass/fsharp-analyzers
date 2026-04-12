/// Analyzer that flags restricted function calls based on editorconfig configuration.
/// Merges functionality from banned functions, banned call patterns, and unsafe dynamic arg detection.
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

/// Get the dotted name from a function expression, e.g. "Task.WhenAll" or "Attr.type'"
let private getFuncName (expr: SynExpr) : string option =
    match expr with
    | SynExpr.Ident id -> Some id.idText
    | SynExpr.LongIdent(longDotId = SynLongIdent(id = ids)) ->
        ids |> List.map (fun i -> i.idText) |> String.concat "." |> Some
    | _ -> None

/// Check if a function name matches a configured name (supports suffix matching for qualified names).
/// e.g. "Task.WhenAll" matches both "Task.WhenAll" and "System.Threading.Tasks.Task.WhenAll"
let private matchesFuncName (configuredName: string) (actualName: string) =
    actualName = configuredName || actualName.EndsWith("." + configuredName)

/// Check if a string literal argument matches
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

/// Recursively walk expressions looking for restricted calls
let rec private walkExpr (config: Config) (diagnostics: ResizeArray<Diagnostic>) (expr: SynExpr) =
    // Check SynExpr.App for all three modes
    match expr with
    | SynExpr.App(funcExpr = funcExpr; argExpr = argExpr) ->
        match getFuncName funcExpr with
        | Some name ->
            // Mode 1: Banned function called as application
            if config.BannedFunctions |> Set.exists (fun banned -> matchesFuncName banned name) then
                diagnostics.Add(
                    { Range = expr.Range
                      Message =
                        $"Banned function call '%s{name}' is not allowed. Add '// %s{Code}:ok' with a reason to suppress." }
                )

            // Mode 2: Banned call pattern
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

            // Mode 3: Unsafe dynamic arg
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

    // Recurse into child expressions
    match expr with
    | SynExpr.App(funcExpr = func; argExpr = arg) ->
        walkExpr config diagnostics func
        walkExpr config diagnostics arg
    | SynExpr.Paren(expr = inner) -> walkExpr config diagnostics inner
    | SynExpr.Typed(expr = inner) -> walkExpr config diagnostics inner
    | SynExpr.Tuple(exprs = exprs) ->
        for e in exprs do
            walkExpr config diagnostics e
    | SynExpr.ArrayOrList(exprs = exprs) ->
        for e in exprs do
            walkExpr config diagnostics e
    | SynExpr.Record(copyInfo = copyExprOpt; recordFields = fields) ->
        match copyExprOpt with
        | Some(e, _) -> walkExpr config diagnostics e
        | None -> ()

        for field in fields do
            match field with
            | SynExprRecordField(expr = exprOpt) ->
                match exprOpt with
                | Some e -> walkExpr config diagnostics e
                | None -> ()
    | SynExpr.New(expr = expr) -> walkExpr config diagnostics expr
    | SynExpr.ObjExpr(argOptions = argOpt; bindings = bindings) ->
        match argOpt with
        | Some(e, _) -> walkExpr config diagnostics e
        | None -> ()

        for binding in bindings do
            walkBinding config diagnostics binding
    | SynExpr.While(whileExpr = cond; doExpr = body) ->
        walkExpr config diagnostics cond
        walkExpr config diagnostics body
    | SynExpr.For(identBody = start; toBody = finish; doBody = body) ->
        walkExpr config diagnostics start
        walkExpr config diagnostics finish
        walkExpr config diagnostics body
    | SynExpr.ForEach(enumExpr = enumExpr; bodyExpr = body) ->
        walkExpr config diagnostics enumExpr
        walkExpr config diagnostics body
    | SynExpr.ArrayOrListComputed(expr = expr) -> walkExpr config diagnostics expr
    | SynExpr.ComputationExpr(expr = expr) -> walkExpr config diagnostics expr
    | SynExpr.Lambda(body = body) -> walkExpr config diagnostics body
    | SynExpr.Assert(expr = expr) -> walkExpr config diagnostics expr
    | SynExpr.Match(clauses = clauses)
    | SynExpr.MatchLambda(matchClauses = clauses)
    | SynExpr.MatchBang(clauses = clauses) ->
        for clause in clauses do
            match clause with
            | SynMatchClause(resultExpr = body) -> walkExpr config diagnostics body
    | SynExpr.LetOrUse(bindings = bindings; body = body) ->
        for binding in bindings do
            walkBinding config diagnostics binding

        walkExpr config diagnostics body
    | SynExpr.TryWith(tryExpr = body; withCases = clauses) ->
        walkExpr config diagnostics body

        for clause in clauses do
            match clause with
            | SynMatchClause(resultExpr = body) -> walkExpr config diagnostics body
    | SynExpr.TryFinally(tryExpr = body; finallyExpr = finallyExpr) ->
        walkExpr config diagnostics body
        walkExpr config diagnostics finallyExpr
    | SynExpr.Lazy(expr = expr) -> walkExpr config diagnostics expr
    | SynExpr.Sequential(expr1 = e1; expr2 = e2) ->
        walkExpr config diagnostics e1
        walkExpr config diagnostics e2
    | SynExpr.IfThenElse(ifExpr = cond; thenExpr = thenExpr; elseExpr = elseExprOpt) ->
        walkExpr config diagnostics cond
        walkExpr config diagnostics thenExpr

        match elseExprOpt with
        | Some e -> walkExpr config diagnostics e
        | None -> ()
    | SynExpr.LongIdentSet(expr = expr) -> walkExpr config diagnostics expr
    | SynExpr.DotGet(expr = expr) -> walkExpr config diagnostics expr
    | SynExpr.DotSet(targetExpr = target; rhsExpr = value) ->
        walkExpr config diagnostics target
        walkExpr config diagnostics value
    | SynExpr.Set(targetExpr = target; rhsExpr = value) ->
        walkExpr config diagnostics target
        walkExpr config diagnostics value
    | SynExpr.DotIndexedGet(objectExpr = expr; indexArgs = indexArgs) ->
        walkExpr config diagnostics expr
        walkExpr config diagnostics indexArgs
    | SynExpr.DotIndexedSet(objectExpr = target; indexArgs = indexArgs; valueExpr = value) ->
        walkExpr config diagnostics target
        walkExpr config diagnostics indexArgs
        walkExpr config diagnostics value
    | SynExpr.NamedIndexedPropertySet(expr1 = e1; expr2 = e2) ->
        walkExpr config diagnostics e1
        walkExpr config diagnostics e2
    | SynExpr.DotNamedIndexedPropertySet(targetExpr = target; argExpr = e1; rhsExpr = e2) ->
        walkExpr config diagnostics target
        walkExpr config diagnostics e1
        walkExpr config diagnostics e2
    | SynExpr.TypeTest(expr = expr) -> walkExpr config diagnostics expr
    | SynExpr.Upcast(expr = expr) -> walkExpr config diagnostics expr
    | SynExpr.Downcast(expr = expr) -> walkExpr config diagnostics expr
    | SynExpr.InferredUpcast(expr = expr) -> walkExpr config diagnostics expr
    | SynExpr.InferredDowncast(expr = expr) -> walkExpr config diagnostics expr
    | SynExpr.AddressOf(expr = expr) -> walkExpr config diagnostics expr
    | SynExpr.JoinIn(lhsExpr = e1; rhsExpr = e2) ->
        walkExpr config diagnostics e1
        walkExpr config diagnostics e2
    | SynExpr.YieldOrReturn(expr = expr) -> walkExpr config diagnostics expr
    | SynExpr.YieldOrReturnFrom(expr = expr) -> walkExpr config diagnostics expr
    | SynExpr.DoBang(expr = expr) -> walkExpr config diagnostics expr
    | SynExpr.TraitCall(argExpr = expr) -> walkExpr config diagnostics expr
    | SynExpr.IndexFromEnd(expr = expr) -> walkExpr config diagnostics expr
    | SynExpr.IndexRange(expr1 = e1Opt; expr2 = e2Opt) ->
        match e1Opt with
        | Some e -> walkExpr config diagnostics e
        | None -> ()

        match e2Opt with
        | Some e -> walkExpr config diagnostics e
        | None -> ()
    | SynExpr.DebugPoint(innerExpr = expr) -> walkExpr config diagnostics expr
    | SynExpr.InterpolatedString(contents = parts) ->
        for part in parts do
            match part with
            | SynInterpolatedStringPart.FillExpr(fillExpr = expr) -> walkExpr config diagnostics expr
            | SynInterpolatedStringPart.String _ -> ()
    | _ -> ()

and private walkBinding (config: Config) (diagnostics: ResizeArray<Diagnostic>) (binding: SynBinding) =
    match binding with
    | SynBinding(expr = body) -> walkExpr config diagnostics body

let rec private walkModuleDecl
    (config: Config)
    (diagnostics: ResizeArray<Diagnostic>)
    (decl: SynModuleDecl)
    =
    match decl with
    | SynModuleDecl.Let(bindings = bindings) ->
        for binding in bindings do
            walkBinding config diagnostics binding
    | SynModuleDecl.Expr(expr = expr) -> walkExpr config diagnostics expr
    | SynModuleDecl.NestedModule(decls = decls) ->
        for d in decls do
            walkModuleDecl config diagnostics d
    | SynModuleDecl.Types(typeDefns = typeDefs) ->
        for typeDef in typeDefs do
            walkTypeDef config diagnostics typeDef
    | _ -> ()

and private walkTypeDef
    (config: Config)
    (diagnostics: ResizeArray<Diagnostic>)
    (typeDef: SynTypeDefn)
    =
    match typeDef with
    | SynTypeDefn(members = members) ->
        for mem in members do
            walkMemberDefn config diagnostics mem

and private walkMemberDefn
    (config: Config)
    (diagnostics: ResizeArray<Diagnostic>)
    (mem: SynMemberDefn)
    =
    match mem with
    | SynMemberDefn.Member(memberDefn = binding) -> walkBinding config diagnostics binding
    | SynMemberDefn.LetBindings(bindings = bindings) ->
        for binding in bindings do
            walkBinding config diagnostics binding
    | SynMemberDefn.GetSetMember(memberDefnForGet = getBinding; memberDefnForSet = setBinding) ->
        match getBinding with
        | Some b -> walkBinding config diagnostics b
        | None -> ()

        match setBinding with
        | Some b -> walkBinding config diagnostics b
        | None -> ()
    | SynMemberDefn.AutoProperty(synExpr = expr) -> walkExpr config diagnostics expr
    | _ -> ()

/// Walk the entire parse tree and collect diagnostics
let private walkParseTree (config: Config) (parseTree: ParsedInput) : Diagnostic list =
    let diagnostics = ResizeArray<Diagnostic>()

    match parseTree with
    | ParsedInput.ImplFile(ParsedImplFileInput(contents = modules)) ->
        for moduleOrNs in modules do
            match moduleOrNs with
            | SynModuleOrNamespace(decls = decls) ->
                for decl in decls do
                    walkModuleDecl config diagnostics decl
    | ParsedInput.SigFile _ -> ()

    diagnostics |> Seq.toList

/// Analyze a context with the given config. Exposed for testing.
let analyze (config: Config) (context: CliContext) : Message list =
    if isConfigEmpty config then
        []
    else
        let diagnostics = walkParseTree config context.ParseFileResults.ParseTree

        diagnostics
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
