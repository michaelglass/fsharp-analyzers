/// Analyzer that flags raw SQL strings in source files.
/// Prefer type-safe query builders instead of raw SQL strings.
/// Code: MGA-RAWSQL-001
module MichaelGlass.FSharp.Analyzers.RawSqlAnalyzer

open System
open FSharp.Analyzers.SDK
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text

/// SQL keywords that indicate a raw SQL string (case-insensitive, checked after trim)
let private sqlKeywords =
    [| "SELECT "
       "INSERT "
       "UPDATE "
       "DELETE "
       "WITH "
       "CREATE "
       "ALTER "
       "DROP "
       "TRUNCATE " |]

/// Check if a string looks like a raw SQL query
let private looksLikeSql (s: string) =
    let trimmed = s.TrimStart()

    sqlKeywords
    |> Array.exists (fun kw -> trimmed.StartsWith(kw, StringComparison.OrdinalIgnoreCase))

/// Check if a file is excluded via editorconfig property mga_rawsql_excluded_files
let private isFileExcluded (fileName: string) =
    let excludedFiles =
        EditorConfig.getListProperty fileName "mga_rawsql_excluded_files"
        |> Set.ofList

    if Set.isEmpty excludedFiles then
        false
    else
        let currentFile = System.IO.Path.GetFileName(fileName)
        Set.contains currentFile excludedFiles

/// Recursively walk expressions looking for SQL string literals
let rec private walkExpr (ranges: ResizeArray<range>) (expr: SynExpr) =
    match expr with
    | SynExpr.Const(SynConst.String(text = text), range) when looksLikeSql text -> ranges.Add(range)
    | SynExpr.InterpolatedString(contents = parts; range = range) ->
        // Check if the first string part starts with SQL
        let firstText =
            parts
            |> List.tryPick (fun part ->
                match part with
                | SynInterpolatedStringPart.String(text, _) when text.Length > 0 -> Some text
                | _ -> None)

        match firstText with
        | Some text when looksLikeSql text -> ranges.Add(range)
        | _ -> ()

        // Still walk fill expressions for nested cases
        for part in parts do
            match part with
            | SynInterpolatedStringPart.FillExpr(fillExpr = expr) -> walkExpr ranges expr
            | SynInterpolatedStringPart.String _ -> ()
    | SynExpr.App(funcExpr = func; argExpr = arg) ->
        walkExpr ranges func
        walkExpr ranges arg
    | SynExpr.Paren(expr = inner) -> walkExpr ranges inner
    | SynExpr.Typed(expr = inner) -> walkExpr ranges inner
    | SynExpr.Tuple(exprs = exprs) ->
        for e in exprs do
            walkExpr ranges e
    | SynExpr.ArrayOrList(exprs = exprs) ->
        for e in exprs do
            walkExpr ranges e
    | SynExpr.Record(copyInfo = copyExprOpt; recordFields = fields) ->
        match copyExprOpt with
        | Some(e, _) -> walkExpr ranges e
        | None -> ()

        for field in fields do
            match field with
            | SynExprRecordField(expr = exprOpt) ->
                match exprOpt with
                | Some e -> walkExpr ranges e
                | None -> ()
    | SynExpr.New(expr = expr) -> walkExpr ranges expr
    | SynExpr.ObjExpr(argOptions = argOpt; bindings = bindings) ->
        match argOpt with
        | Some(e, _) -> walkExpr ranges e
        | None -> ()

        for binding in bindings do
            walkBinding ranges binding
    | SynExpr.While(whileExpr = cond; doExpr = body) ->
        walkExpr ranges cond
        walkExpr ranges body
    | SynExpr.For(identBody = start; toBody = finish; doBody = body) ->
        walkExpr ranges start
        walkExpr ranges finish
        walkExpr ranges body
    | SynExpr.ForEach(enumExpr = enumExpr; bodyExpr = body) ->
        walkExpr ranges enumExpr
        walkExpr ranges body
    | SynExpr.ArrayOrListComputed(expr = expr) -> walkExpr ranges expr
    | SynExpr.ComputationExpr(expr = expr) -> walkExpr ranges expr
    | SynExpr.Lambda(body = body) -> walkExpr ranges body
    | SynExpr.Assert(expr = expr) -> walkExpr ranges expr
    | SynExpr.Match(clauses = clauses)
    | SynExpr.MatchLambda(matchClauses = clauses)
    | SynExpr.MatchBang(clauses = clauses) ->
        for clause in clauses do
            match clause with
            | SynMatchClause(resultExpr = body) -> walkExpr ranges body
    | SynExpr.LetOrUse(bindings = bindings; body = body) ->
        for binding in bindings do
            walkBinding ranges binding

        walkExpr ranges body
    | SynExpr.TryWith(tryExpr = body; withCases = clauses) ->
        walkExpr ranges body

        for clause in clauses do
            match clause with
            | SynMatchClause(resultExpr = body) -> walkExpr ranges body
    | SynExpr.TryFinally(tryExpr = body; finallyExpr = finallyExpr) ->
        walkExpr ranges body
        walkExpr ranges finallyExpr
    | SynExpr.Lazy(expr = expr) -> walkExpr ranges expr
    | SynExpr.Sequential(expr1 = e1; expr2 = e2) ->
        walkExpr ranges e1
        walkExpr ranges e2
    | SynExpr.IfThenElse(ifExpr = cond; thenExpr = thenExpr; elseExpr = elseExprOpt) ->
        walkExpr ranges cond
        walkExpr ranges thenExpr

        match elseExprOpt with
        | Some e -> walkExpr ranges e
        | None -> ()
    | SynExpr.LongIdentSet(expr = expr) -> walkExpr ranges expr
    | SynExpr.DotGet(expr = expr) -> walkExpr ranges expr
    | SynExpr.DotSet(targetExpr = target; rhsExpr = value) ->
        walkExpr ranges target
        walkExpr ranges value
    | SynExpr.Set(targetExpr = target; rhsExpr = value) ->
        walkExpr ranges target
        walkExpr ranges value
    | SynExpr.DotIndexedGet(objectExpr = expr; indexArgs = indexArgs) ->
        walkExpr ranges expr
        walkExpr ranges indexArgs
    | SynExpr.DotIndexedSet(objectExpr = target; indexArgs = indexArgs; valueExpr = value) ->
        walkExpr ranges target
        walkExpr ranges indexArgs
        walkExpr ranges value
    | SynExpr.NamedIndexedPropertySet(expr1 = e1; expr2 = e2) ->
        walkExpr ranges e1
        walkExpr ranges e2
    | SynExpr.DotNamedIndexedPropertySet(targetExpr = target; argExpr = e1; rhsExpr = e2) ->
        walkExpr ranges target
        walkExpr ranges e1
        walkExpr ranges e2
    | SynExpr.TypeTest(expr = expr) -> walkExpr ranges expr
    | SynExpr.Upcast(expr = expr) -> walkExpr ranges expr
    | SynExpr.Downcast(expr = expr) -> walkExpr ranges expr
    | SynExpr.InferredUpcast(expr = expr) -> walkExpr ranges expr
    | SynExpr.InferredDowncast(expr = expr) -> walkExpr ranges expr
    | SynExpr.AddressOf(expr = expr) -> walkExpr ranges expr
    | SynExpr.JoinIn(lhsExpr = e1; rhsExpr = e2) ->
        walkExpr ranges e1
        walkExpr ranges e2
    | SynExpr.YieldOrReturn(expr = expr) -> walkExpr ranges expr
    | SynExpr.YieldOrReturnFrom(expr = expr) -> walkExpr ranges expr
    | SynExpr.DoBang(expr = expr) -> walkExpr ranges expr
    | SynExpr.TraitCall(argExpr = expr) -> walkExpr ranges expr
    | SynExpr.IndexFromEnd(expr = expr) -> walkExpr ranges expr
    | SynExpr.IndexRange(expr1 = e1Opt; expr2 = e2Opt) ->
        match e1Opt with
        | Some e -> walkExpr ranges e
        | None -> ()

        match e2Opt with
        | Some e -> walkExpr ranges e
        | None -> ()
    | SynExpr.DebugPoint(innerExpr = expr) -> walkExpr ranges expr
    | _ -> ()

and private walkBinding (ranges: ResizeArray<range>) (binding: SynBinding) =
    match binding with
    | SynBinding(expr = body) -> walkExpr ranges body

let rec private walkModuleDecl (ranges: ResizeArray<range>) (decl: SynModuleDecl) =
    match decl with
    | SynModuleDecl.Let(bindings = bindings) ->
        for binding in bindings do
            walkBinding ranges binding
    | SynModuleDecl.Expr(expr = expr) -> walkExpr ranges expr
    | SynModuleDecl.NestedModule(decls = decls) ->
        for d in decls do
            walkModuleDecl ranges d
    | SynModuleDecl.Types(typeDefns = typeDefs) ->
        for typeDef in typeDefs do
            walkTypeDef ranges typeDef
    | _ -> ()

and private walkTypeDef (ranges: ResizeArray<range>) (typeDef: SynTypeDefn) =
    match typeDef with
    | SynTypeDefn(members = members) ->
        for mem in members do
            walkMemberDefn ranges mem

and private walkMemberDefn (ranges: ResizeArray<range>) (mem: SynMemberDefn) =
    match mem with
    | SynMemberDefn.Member(memberDefn = binding) -> walkBinding ranges binding
    | SynMemberDefn.LetBindings(bindings = bindings) ->
        for binding in bindings do
            walkBinding ranges binding
    | SynMemberDefn.GetSetMember(memberDefnForGet = getBinding; memberDefnForSet = setBinding) ->
        match getBinding with
        | Some b -> walkBinding ranges b
        | None -> ()

        match setBinding with
        | Some b -> walkBinding ranges b
        | None -> ()
    | SynMemberDefn.AutoProperty(synExpr = expr) -> walkExpr ranges expr
    | _ -> ()

/// Walk the entire parse tree and collect ranges of raw SQL strings
let private walkParseTree (parseTree: ParsedInput) : range list =
    let ranges = ResizeArray<range>()

    match parseTree with
    | ParsedInput.ImplFile(ParsedImplFileInput(contents = modules)) ->
        for moduleOrNs in modules do
            match moduleOrNs with
            | SynModuleOrNamespace(decls = decls) ->
                for decl in decls do
                    walkModuleDecl ranges decl
    | ParsedInput.SigFile _ -> ()

    ranges |> Seq.toList

[<CliAnalyzer("RawSqlAnalyzer",
              "Flags raw SQL strings in source files. Prefer type-safe query builders instead of raw SQL.")>]
let rawSqlAnalyzer: Analyzer<CliContext> =
    fun (context: CliContext) ->
        async {
            if isFileExcluded context.FileName then
                return []
            else
                let ranges = walkParseTree context.ParseFileResults.ParseTree

                let messages =
                    ranges
                    |> List.filter (fun range ->
                        not (Suppression.isLineSuppressed context.SourceText range "MGA-RAWSQL-001"))
                    |> List.map (fun range ->
                        { Type = "Raw SQL string"
                          Message =
                            "Raw SQL string detected — prefer type-safe query builders instead of raw SQL. Add '// MGA-RAWSQL-001:ok' with a reason to suppress."
                          Code = "MGA-RAWSQL-001"
                          Severity = Severity.Warning
                          Range = range
                          Fixes = [] })

                return messages
        }
