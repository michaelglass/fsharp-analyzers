/// Analyzer that forbids catch-all wildcards on discriminated union types.
/// Code: MGA-WILDCARD-001
module MichaelGlass.FSharp.Analyzers.WildcardAnalyzer

open FSharp.Analyzers.SDK
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text

/// Default types where wildcard matching is allowed (F# stdlib).
let private defaultAllowedTypes =
    Set.ofList
        [ "Microsoft.FSharp.Core.option`1"
          "Microsoft.FSharp.Core.voption`1"
          "Microsoft.FSharp.Core.FSharpResult`2"
          "Microsoft.FSharp.Core.FSharpChoice`2"
          "Microsoft.FSharp.Core.FSharpChoice`3"
          "Microsoft.FSharp.Core.FSharpChoice`4"
          "Microsoft.FSharp.Core.FSharpChoice`5"
          "Microsoft.FSharp.Core.FSharpChoice`6"
          "Microsoft.FSharp.Core.FSharpChoice`7" ]

/// Get the full set of allowed types: defaults + editorconfig overrides.
let private getAllowedTypes (fileName: string) =
    let extra = EditorConfig.getListProperty fileName "mga_wildcard_allowed_types"
    Set.union defaultAllowedTypes (Set.ofList extra)

/// Unwrap parentheses from a pattern to get the inner pattern
let rec unwrapParen (pat: SynPat) =
    match pat with
    | SynPat.Paren(inner, _) -> unwrapParen inner
    | _ -> pat

/// Check if a pattern is a top-level wildcard (unwrapping parens)
let isWildcard (pat: SynPat) =
    match unwrapParen pat with
    | SynPat.Wild _ -> true
    | _ -> false

/// Check if a pattern is a LongIdent pattern (DU case match)
let isLongIdent (pat: SynPat) =
    match unwrapParen pat with
    | SynPat.LongIdent _ -> true
    | _ -> false

/// Get the range of a wildcard pattern
let getWildcardRange (pat: SynPat) =
    match unwrapParen pat with
    | SynPat.Wild range -> Some range
    | _ -> None

/// Get the pattern from a SynMatchClause
let getClausePattern (clause: SynMatchClause) =
    match clause with
    | SynMatchClause(pat = pat) -> pat

/// Get the body expression from a SynMatchClause
let getClauseBody (clause: SynMatchClause) =
    match clause with
    | SynMatchClause(resultExpr = body) -> body

/// Check if any clause has a LongIdent pattern (indicating DU case matching)
let hasLongIdentSibling (clauses: SynMatchClause list) =
    clauses |> List.exists (fun c -> isLongIdent (getClausePattern c))

/// Find the first LongIdent pattern's SynLongIdent from match clauses
let getFirstLongIdentInfo (clauses: SynMatchClause list) : SynLongIdent option =
    clauses
    |> List.tryPick (fun c ->
        match unwrapParen (getClausePattern c) with
        | SynPat.LongIdent(longDotId = synLongIdent) -> Some synLongIdent
        | _ -> None)

/// Try to resolve the union type from a SynLongIdent using typed check results
let tryResolveUnionType
    (checkResults: FSharpCheckFileResults)
    (sourceText: ISourceText)
    (synLongIdent: SynLongIdent)
    : string option =
    try
        let idents = synLongIdent.LongIdent
        let lastIdent = idents |> List.last
        let identRange = lastIdent.idRange
        let line = identRange.EndLine
        let col = identRange.EndColumn
        let lineText = sourceText.GetLineString(line - 1)
        let names = idents |> List.map (fun id -> id.idText)

        match checkResults.GetSymbolUseAtLocation(line, col, lineText, names) with
        | Some symbolUse ->
            match symbolUse.Symbol with
            | :? FSharpUnionCase as uc ->
                let typeDef = uc.ReturnType.TypeDefinition

                match typeDef.TryFullName with
                | Some _ as result -> result
                | None ->
                    // Generic type abbreviations (e.g. option<'T>) don't have TryFullName.
                    // Fall back to AccessPath + CompiledName.
                    try
                        Some(typeDef.AccessPath + "." + typeDef.CompiledName)
                    with _ ->
                        None
            | _ -> None
        | None -> None
    with _ ->
        None

/// Check if the wildcard should be flagged.
/// Returns true (should flag) only when the type IS a resolved F# DU AND is NOT in the allowlist.
/// Unresolvable types (enums, .NET types, etc.) are not flagged.
let shouldFlagWildcard
    (checkResults: FSharpCheckFileResults)
    (sourceText: ISourceText)
    (allowedTypes: Set<string>)
    (clauses: SynMatchClause list)
    : bool =
    match getFirstLongIdentInfo clauses with
    | Some synLongIdent ->
        match tryResolveUnionType checkResults sourceText synLongIdent with
        | Some fullName -> not (Set.contains fullName allowedTypes)
        | None -> false // Can't resolve type (enum, .NET type, etc.) -- don't flag
    | None -> false

/// Context threaded through the recursive walk
[<NoComparison; NoEquality>]
type private WalkContext =
    { CheckResults: FSharpCheckFileResults
      SourceText: ISourceText
      AllowedTypes: Set<string>
      Ranges: ResizeArray<range> }

/// Find all wildcard patterns in match clauses where siblings have LongIdent patterns
let private findProblematicWildcards (ctx: WalkContext) (clauses: SynMatchClause list) : range list =
    if not (hasLongIdentSibling clauses) then
        []
    elif not (shouldFlagWildcard ctx.CheckResults ctx.SourceText ctx.AllowedTypes clauses) then
        []
    else
        clauses
        |> List.choose (fun c ->
            let pat = getClausePattern c
            if isWildcard pat then getWildcardRange pat else None)

/// Recursively walk an expression and collect wildcard ranges
let rec private walkExpr (ctx: WalkContext) (expr: SynExpr) =
    match expr with
    | SynExpr.Match(clauses = clauses)
    | SynExpr.MatchLambda(matchClauses = clauses)
    | SynExpr.MatchBang(clauses = clauses) ->
        let problematic = findProblematicWildcards ctx clauses
        ctx.Ranges.AddRange(problematic)

        for clause in clauses do
            walkExpr ctx (getClauseBody clause)
    | SynExpr.Paren(expr = inner) -> walkExpr ctx inner
    | SynExpr.Typed(expr = inner) -> walkExpr ctx inner
    | SynExpr.Tuple(exprs = exprs) ->
        for e in exprs do
            walkExpr ctx e
    | SynExpr.ArrayOrList(exprs = exprs) ->
        for e in exprs do
            walkExpr ctx e
    | SynExpr.Record(copyInfo = copyExprOpt; recordFields = fields) ->
        match copyExprOpt with
        | Some(e, _) -> walkExpr ctx e
        | None -> ()

        for field in fields do
            match field with
            | SynExprRecordField(expr = exprOpt) ->
                match exprOpt with
                | Some e -> walkExpr ctx e
                | None -> ()
    | SynExpr.New(expr = expr) -> walkExpr ctx expr
    | SynExpr.ObjExpr(argOptions = argOpt; bindings = bindings) ->
        match argOpt with
        | Some(e, _) -> walkExpr ctx e
        | None -> ()

        for binding in bindings do
            walkBinding ctx binding
    | SynExpr.While(whileExpr = cond; doExpr = body) ->
        walkExpr ctx cond
        walkExpr ctx body
    | SynExpr.For(identBody = start; toBody = finish; doBody = body) ->
        walkExpr ctx start
        walkExpr ctx finish
        walkExpr ctx body
    | SynExpr.ForEach(enumExpr = enumExpr; bodyExpr = body) ->
        walkExpr ctx enumExpr
        walkExpr ctx body
    | SynExpr.ArrayOrListComputed(expr = expr) -> walkExpr ctx expr
    | SynExpr.ComputationExpr(expr = expr) -> walkExpr ctx expr
    | SynExpr.Lambda(body = body) -> walkExpr ctx body
    | SynExpr.Assert(expr = expr) -> walkExpr ctx expr
    | SynExpr.App(funcExpr = func; argExpr = arg) ->
        walkExpr ctx func
        walkExpr ctx arg
    | SynExpr.LetOrUse(bindings = bindings; body = body) ->
        for binding in bindings do
            walkBinding ctx binding

        walkExpr ctx body
    | SynExpr.TryWith(tryExpr = body; withCases = clauses) ->
        walkExpr ctx body

        for clause in clauses do
            walkExpr ctx (getClauseBody clause)
    | SynExpr.TryFinally(tryExpr = body; finallyExpr = finallyExpr) ->
        walkExpr ctx body
        walkExpr ctx finallyExpr
    | SynExpr.Lazy(expr = expr) -> walkExpr ctx expr
    | SynExpr.Sequential(expr1 = e1; expr2 = e2) ->
        walkExpr ctx e1
        walkExpr ctx e2
    | SynExpr.IfThenElse(ifExpr = cond; thenExpr = thenExpr; elseExpr = elseExprOpt) ->
        walkExpr ctx cond
        walkExpr ctx thenExpr

        match elseExprOpt with
        | Some e -> walkExpr ctx e
        | None -> ()
    | SynExpr.LongIdentSet(expr = expr) -> walkExpr ctx expr
    | SynExpr.DotGet(expr = expr) -> walkExpr ctx expr
    | SynExpr.DotSet(targetExpr = target; rhsExpr = value) ->
        walkExpr ctx target
        walkExpr ctx value
    | SynExpr.Set(targetExpr = target; rhsExpr = value) ->
        walkExpr ctx target
        walkExpr ctx value
    | SynExpr.DotIndexedGet(objectExpr = expr; indexArgs = indexArgs) ->
        walkExpr ctx expr
        walkExpr ctx indexArgs
    | SynExpr.DotIndexedSet(objectExpr = target; indexArgs = indexArgs; valueExpr = value) ->
        walkExpr ctx target
        walkExpr ctx indexArgs
        walkExpr ctx value
    | SynExpr.NamedIndexedPropertySet(expr1 = e1; expr2 = e2) ->
        walkExpr ctx e1
        walkExpr ctx e2
    | SynExpr.DotNamedIndexedPropertySet(targetExpr = target; argExpr = e1; rhsExpr = e2) ->
        walkExpr ctx target
        walkExpr ctx e1
        walkExpr ctx e2
    | SynExpr.TypeTest(expr = expr) -> walkExpr ctx expr
    | SynExpr.Upcast(expr = expr) -> walkExpr ctx expr
    | SynExpr.Downcast(expr = expr) -> walkExpr ctx expr
    | SynExpr.InferredUpcast(expr = expr) -> walkExpr ctx expr
    | SynExpr.InferredDowncast(expr = expr) -> walkExpr ctx expr
    | SynExpr.AddressOf(expr = expr) -> walkExpr ctx expr
    | SynExpr.JoinIn(lhsExpr = e1; rhsExpr = e2) ->
        walkExpr ctx e1
        walkExpr ctx e2
    | SynExpr.YieldOrReturn(expr = expr) -> walkExpr ctx expr
    | SynExpr.YieldOrReturnFrom(expr = expr) -> walkExpr ctx expr
    | SynExpr.DoBang(expr = expr) -> walkExpr ctx expr
    | SynExpr.TraitCall(argExpr = expr) -> walkExpr ctx expr
    | SynExpr.IndexFromEnd(expr = expr) -> walkExpr ctx expr
    | SynExpr.IndexRange(expr1 = e1Opt; expr2 = e2Opt) ->
        match e1Opt with
        | Some e -> walkExpr ctx e
        | None -> ()

        match e2Opt with
        | Some e -> walkExpr ctx e
        | None -> ()
    | SynExpr.DebugPoint(innerExpr = expr) -> walkExpr ctx expr
    | SynExpr.InterpolatedString(contents = parts) ->
        for part in parts do
            match part with
            | SynInterpolatedStringPart.FillExpr(fillExpr = expr) -> walkExpr ctx expr
            | SynInterpolatedStringPart.String _ -> ()
    | _ -> ()

and private walkBinding (ctx: WalkContext) (binding: SynBinding) =
    match binding with
    | SynBinding(expr = body) -> walkExpr ctx body

let rec private walkModuleDecl (ctx: WalkContext) (decl: SynModuleDecl) =
    match decl with
    | SynModuleDecl.Let(bindings = bindings) ->
        for binding in bindings do
            walkBinding ctx binding
    | SynModuleDecl.Expr(expr = expr) -> walkExpr ctx expr
    | SynModuleDecl.NestedModule(decls = decls) ->
        for d in decls do
            walkModuleDecl ctx d
    | SynModuleDecl.Types(typeDefns = typeDefs) ->
        for typeDef in typeDefs do
            walkTypeDef ctx typeDef
    | _ -> ()

and private walkTypeDef (ctx: WalkContext) (typeDef: SynTypeDefn) =
    match typeDef with
    | SynTypeDefn(members = members) ->
        for mem in members do
            walkMemberDefn ctx mem

and private walkMemberDefn (ctx: WalkContext) (mem: SynMemberDefn) =
    match mem with
    | SynMemberDefn.Member(memberDefn = binding) -> walkBinding ctx binding
    | SynMemberDefn.LetBindings(bindings = bindings) ->
        for binding in bindings do
            walkBinding ctx binding
    | SynMemberDefn.GetSetMember(memberDefnForGet = getBinding; memberDefnForSet = setBinding) ->
        match getBinding with
        | Some b -> walkBinding ctx b
        | None -> ()

        match setBinding with
        | Some b -> walkBinding ctx b
        | None -> ()
    | SynMemberDefn.AutoProperty(synExpr = expr) -> walkExpr ctx expr
    | _ -> ()

/// Walk the entire parse tree and collect wildcard ranges
let private walkParseTree (ctx: WalkContext) (parseTree: ParsedInput) : range list =
    match parseTree with
    | ParsedInput.ImplFile(ParsedImplFileInput(contents = modules)) ->
        for moduleOrNs in modules do
            match moduleOrNs with
            | SynModuleOrNamespace(decls = decls) ->
                for decl in decls do
                    walkModuleDecl ctx decl
    | ParsedInput.SigFile _ -> ()

    ctx.Ranges |> Seq.toList

[<CliAnalyzer("WildcardOnDUAnalyzer",
              "Forbids catch-all wildcards on DU match expressions")>]
let wildcardAnalyzer: Analyzer<CliContext> =
    fun (context: CliContext) ->
        async {
            let allowedTypes = getAllowedTypes context.FileName

            let ctx =
                { CheckResults = context.CheckFileResults
                  SourceText = context.SourceText
                  AllowedTypes = allowedTypes
                  Ranges = ResizeArray<range>() }

            let wildcardRanges = walkParseTree ctx context.ParseFileResults.ParseTree

            let messages =
                wildcardRanges
                |> List.filter (fun range ->
                    not (Suppression.isLineSuppressed context.SourceText range "MGA-WILDCARD-001"))
                |> List.map (fun range ->
                    { Type = "Wildcard on DU"
                      Message =
                        "Catch-all pattern '| _ ->' on discriminated union type hides exhaustiveness checking. List all cases explicitly, or add '// MGA-WILDCARD-001:ok' to suppress."
                      Code = "MGA-WILDCARD-001"
                      Severity = Severity.Warning
                      Range = range
                      Fixes = [] })

            return messages
        }
