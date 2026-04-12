/// Analyzer that flags try/with blocks where catch handlers don't call
/// a configured error-reporting function.
/// Opt-in: reads mga_error_reporting_functions from .editorconfig.
/// Code: MGA-ERROR-REPORT-001
module MichaelGlass.FSharp.Analyzers.ErrorReportingAnalyzer

open FSharp.Analyzers.SDK
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text

/// Read the set of required error-reporting function names from .editorconfig.
let private getRequiredFunctions (fileName: string) =
    EditorConfig.getListProperty fileName "mga_error_reporting_functions"
    |> Set.ofList

/// Check if an identifier is one of the required error-reporting functions.
let private isRequiredCall (requiredFunctions: Set<string>) (name: string) =
    Set.contains name requiredFunctions

/// Recursively check if an expression contains a call to any required function.
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

/// Walk expressions looking for TryWith nodes whose handlers lack required calls.
let rec private walkExpr
    (requiredFunctions: Set<string>)
    (ranges: ResizeArray<range>)
    (expr: SynExpr)
    =
    let walk = walkExpr requiredFunctions ranges

    match expr with
    | SynExpr.TryWith(tryExpr = body; withCases = clauses; range = tryWithRange) ->
        // Check each handler individually
        for clause in clauses do
            match clause with
            | SynMatchClause(resultExpr = handlerBody) ->
                if not (containsRequiredCall requiredFunctions handlerBody) then
                    ranges.Add(tryWithRange)

        // Still walk the body and handler bodies for nested try/with
        walk body

        for clause in clauses do
            match clause with
            | SynMatchClause(resultExpr = body) -> walk body
    | SynExpr.App(funcExpr = func; argExpr = arg) ->
        walk func
        walk arg
    | SynExpr.Paren(expr = inner) -> walk inner
    | SynExpr.Typed(expr = inner) -> walk inner
    | SynExpr.Tuple(exprs = exprs) ->
        for e in exprs do
            walk e
    | SynExpr.ArrayOrList(exprs = exprs) ->
        for e in exprs do
            walk e
    | SynExpr.Record(copyInfo = copyExprOpt; recordFields = fields) ->
        match copyExprOpt with
        | Some(e, _) -> walk e
        | None -> ()

        for field in fields do
            match field with
            | SynExprRecordField(expr = exprOpt) ->
                match exprOpt with
                | Some e -> walk e
                | None -> ()
    | SynExpr.New(expr = expr) -> walk expr
    | SynExpr.ObjExpr(argOptions = argOpt; bindings = bindings) ->
        match argOpt with
        | Some(e, _) -> walk e
        | None -> ()

        for binding in bindings do
            walkBinding requiredFunctions ranges binding
    | SynExpr.While(whileExpr = cond; doExpr = body) ->
        walk cond
        walk body
    | SynExpr.For(identBody = start; toBody = finish; doBody = body) ->
        walk start
        walk finish
        walk body
    | SynExpr.ForEach(enumExpr = enumExpr; bodyExpr = body) ->
        walk enumExpr
        walk body
    | SynExpr.ArrayOrListComputed(expr = expr) -> walk expr
    | SynExpr.ComputationExpr(expr = expr) -> walk expr
    | SynExpr.Lambda(body = body) -> walk body
    | SynExpr.Assert(expr = expr) -> walk expr
    | SynExpr.Match(clauses = clauses)
    | SynExpr.MatchLambda(matchClauses = clauses)
    | SynExpr.MatchBang(clauses = clauses) ->
        for clause in clauses do
            match clause with
            | SynMatchClause(resultExpr = body) -> walk body
    | SynExpr.LetOrUse(bindings = bindings; body = body) ->
        for binding in bindings do
            walkBinding requiredFunctions ranges binding

        walk body
    | SynExpr.TryFinally(tryExpr = body; finallyExpr = finallyExpr) ->
        walk body
        walk finallyExpr
    | SynExpr.Lazy(expr = expr) -> walk expr
    | SynExpr.Sequential(expr1 = e1; expr2 = e2) ->
        walk e1
        walk e2
    | SynExpr.IfThenElse(ifExpr = cond; thenExpr = thenExpr; elseExpr = elseExprOpt) ->
        walk cond
        walk thenExpr

        match elseExprOpt with
        | Some e -> walk e
        | None -> ()
    | SynExpr.LongIdentSet(expr = expr) -> walk expr
    | SynExpr.DotGet(expr = expr) -> walk expr
    | SynExpr.DotSet(targetExpr = target; rhsExpr = value) ->
        walk target
        walk value
    | SynExpr.Set(targetExpr = target; rhsExpr = value) ->
        walk target
        walk value
    | SynExpr.DotIndexedGet(objectExpr = expr; indexArgs = indexArgs) ->
        walk expr
        walk indexArgs
    | SynExpr.DotIndexedSet(objectExpr = target; indexArgs = indexArgs; valueExpr = value) ->
        walk target
        walk indexArgs
        walk value
    | SynExpr.NamedIndexedPropertySet(expr1 = e1; expr2 = e2) ->
        walk e1
        walk e2
    | SynExpr.DotNamedIndexedPropertySet(targetExpr = target; argExpr = e1; rhsExpr = e2) ->
        walk target
        walk e1
        walk e2
    | SynExpr.TypeTest(expr = expr) -> walk expr
    | SynExpr.Upcast(expr = expr) -> walk expr
    | SynExpr.Downcast(expr = expr) -> walk expr
    | SynExpr.InferredUpcast(expr = expr) -> walk expr
    | SynExpr.InferredDowncast(expr = expr) -> walk expr
    | SynExpr.AddressOf(expr = expr) -> walk expr
    | SynExpr.JoinIn(lhsExpr = e1; rhsExpr = e2) ->
        walk e1
        walk e2
    | SynExpr.YieldOrReturn(expr = expr) -> walk expr
    | SynExpr.YieldOrReturnFrom(expr = expr) -> walk expr
    | SynExpr.DoBang(expr = expr) -> walk expr
    | SynExpr.TraitCall(argExpr = expr) -> walk expr
    | SynExpr.IndexFromEnd(expr = expr) -> walk expr
    | SynExpr.IndexRange(expr1 = e1Opt; expr2 = e2Opt) ->
        match e1Opt with
        | Some e -> walk e
        | None -> ()

        match e2Opt with
        | Some e -> walk e
        | None -> ()
    | SynExpr.DebugPoint(innerExpr = expr) -> walk expr
    | SynExpr.InterpolatedString(contents = parts) ->
        for part in parts do
            match part with
            | SynInterpolatedStringPart.FillExpr(fillExpr = expr) -> walk expr
            | SynInterpolatedStringPart.String _ -> ()
    | _ -> ()

and private walkBinding (requiredFunctions: Set<string>) (ranges: ResizeArray<range>) (binding: SynBinding) =
    match binding with
    | SynBinding(expr = body) -> walkExpr requiredFunctions ranges body

let rec private walkModuleDecl (requiredFunctions: Set<string>) (ranges: ResizeArray<range>) (decl: SynModuleDecl) =
    match decl with
    | SynModuleDecl.Let(bindings = bindings) ->
        for binding in bindings do
            walkBinding requiredFunctions ranges binding
    | SynModuleDecl.Expr(expr = expr) -> walkExpr requiredFunctions ranges expr
    | SynModuleDecl.NestedModule(decls = decls) ->
        for d in decls do
            walkModuleDecl requiredFunctions ranges d
    | SynModuleDecl.Types(typeDefns = typeDefs) ->
        for typeDef in typeDefs do
            walkTypeDef requiredFunctions ranges typeDef
    | _ -> ()

and private walkTypeDef (requiredFunctions: Set<string>) (ranges: ResizeArray<range>) (typeDef: SynTypeDefn) =
    match typeDef with
    | SynTypeDefn(members = members) ->
        for mem in members do
            walkMemberDefn requiredFunctions ranges mem

and private walkMemberDefn (requiredFunctions: Set<string>) (ranges: ResizeArray<range>) (mem: SynMemberDefn) =
    match mem with
    | SynMemberDefn.Member(memberDefn = binding) -> walkBinding requiredFunctions ranges binding
    | SynMemberDefn.LetBindings(bindings = bindings) ->
        for binding in bindings do
            walkBinding requiredFunctions ranges binding
    | SynMemberDefn.GetSetMember(memberDefnForGet = getBinding; memberDefnForSet = setBinding) ->
        match getBinding with
        | Some b -> walkBinding requiredFunctions ranges b
        | None -> ()

        match setBinding with
        | Some b -> walkBinding requiredFunctions ranges b
        | None -> ()
    | SynMemberDefn.AutoProperty(synExpr = expr) -> walkExpr requiredFunctions ranges expr
    | _ -> ()

/// Walk the entire parse tree and collect ranges of try/with blocks without required calls.
let private walkParseTree (requiredFunctions: Set<string>) (parseTree: ParsedInput) : range list =
    let ranges = ResizeArray<range>()

    match parseTree with
    | ParsedInput.ImplFile(ParsedImplFileInput(contents = modules)) ->
        for moduleOrNs in modules do
            match moduleOrNs with
            | SynModuleOrNamespace(decls = decls) ->
                for decl in decls do
                    walkModuleDecl requiredFunctions ranges decl
    | ParsedInput.SigFile _ -> ()

    ranges |> Seq.toList

/// Core analysis logic, exposed for testing.
/// Takes requiredFunctions explicitly so tests can provide them without editorconfig.
let analyze (requiredFunctions: Set<string>) (context: CliContext) : Message list =
    if Set.isEmpty requiredFunctions then
        []
    else
        let ranges = walkParseTree requiredFunctions context.ParseFileResults.ParseTree

        ranges
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
