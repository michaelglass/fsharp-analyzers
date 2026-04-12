/// <summary>
/// Shared AST traversal utility that walks the full F# untyped parse tree.
/// Analyzers provide a visitor function; this module handles all recursive descent.
/// </summary>
module MichaelGlass.FSharp.Analyzers.AstWalk

open FSharp.Compiler.Syntax

/// <summary>
/// Walks every <see cref="T:FSharp.Compiler.Syntax.SynExpr"/> node in the parse tree.
/// The visitor controls recursion: return true to descend into children,
/// false to prune that subtree.
/// </summary>
/// <param name="visitExpr">Called on each expression. Return false to skip children.</param>
/// <param name="parseTree">The parsed F# source file.</param>
let walkParseTree (visitExpr: SynExpr -> bool) (parseTree: ParsedInput) : unit =
    let rec walkExpr (expr: SynExpr) =
        let recurse = visitExpr expr

        if recurse then
            match expr with
            | SynExpr.App(funcExpr = func; argExpr = arg) ->
                walkExpr func
                walkExpr arg
            | SynExpr.Paren(expr = inner) -> walkExpr inner
            | SynExpr.Typed(expr = inner) -> walkExpr inner
            | SynExpr.Tuple(exprs = exprs) ->
                for e in exprs do
                    walkExpr e
            | SynExpr.ArrayOrList(exprs = exprs) ->
                for e in exprs do
                    walkExpr e
            | SynExpr.Record(copyInfo = copyExprOpt; recordFields = fields) ->
                match copyExprOpt with
                | Some(e, _) -> walkExpr e
                | None -> ()

                for field in fields do
                    match field with
                    | SynExprRecordField(expr = exprOpt) ->
                        match exprOpt with
                        | Some e -> walkExpr e
                        | None -> ()
            | SynExpr.New(expr = expr) -> walkExpr expr
            | SynExpr.ObjExpr(argOptions = argOpt; bindings = bindings) ->
                match argOpt with
                | Some(e, _) -> walkExpr e
                | None -> ()

                for binding in bindings do
                    walkBinding binding
            | SynExpr.While(whileExpr = cond; doExpr = body) ->
                walkExpr cond
                walkExpr body
            | SynExpr.For(identBody = start; toBody = finish; doBody = body) ->
                walkExpr start
                walkExpr finish
                walkExpr body
            | SynExpr.ForEach(enumExpr = enumExpr; bodyExpr = body) ->
                walkExpr enumExpr
                walkExpr body
            | SynExpr.ArrayOrListComputed(expr = expr) -> walkExpr expr
            | SynExpr.ComputationExpr(expr = expr) -> walkExpr expr
            | SynExpr.Lambda(body = body) -> walkExpr body
            | SynExpr.Assert(expr = expr) -> walkExpr expr
            | SynExpr.Match(expr = scrutinee; clauses = clauses)
            | SynExpr.MatchBang(expr = scrutinee; clauses = clauses) ->
                walkExpr scrutinee

                for clause in clauses do
                    match clause with
                    | SynMatchClause(resultExpr = body) -> walkExpr body
            | SynExpr.MatchLambda(matchClauses = clauses) ->
                for clause in clauses do
                    match clause with
                    | SynMatchClause(resultExpr = body) -> walkExpr body
            | SynExpr.LetOrUse(bindings = bindings; body = body) ->
                for binding in bindings do
                    walkBinding binding

                walkExpr body
            | SynExpr.TryWith(tryExpr = body; withCases = clauses) ->
                walkExpr body

                for clause in clauses do
                    match clause with
                    | SynMatchClause(resultExpr = body) -> walkExpr body
            | SynExpr.TryFinally(tryExpr = body; finallyExpr = finallyExpr) ->
                walkExpr body
                walkExpr finallyExpr
            | SynExpr.Lazy(expr = expr) -> walkExpr expr
            | SynExpr.Sequential(expr1 = e1; expr2 = e2) ->
                walkExpr e1
                walkExpr e2
            | SynExpr.IfThenElse(ifExpr = cond; thenExpr = thenExpr; elseExpr = elseExprOpt) ->
                walkExpr cond
                walkExpr thenExpr

                match elseExprOpt with
                | Some e -> walkExpr e
                | None -> ()
            | SynExpr.LongIdentSet(expr = expr) -> walkExpr expr
            | SynExpr.DotGet(expr = expr) -> walkExpr expr
            | SynExpr.DotSet(targetExpr = target; rhsExpr = value) ->
                walkExpr target
                walkExpr value
            | SynExpr.Set(targetExpr = target; rhsExpr = value) ->
                walkExpr target
                walkExpr value
            | SynExpr.DotIndexedGet(objectExpr = expr; indexArgs = indexArgs) ->
                walkExpr expr
                walkExpr indexArgs
            | SynExpr.DotIndexedSet(objectExpr = target; indexArgs = indexArgs; valueExpr = value) ->
                walkExpr target
                walkExpr indexArgs
                walkExpr value
            | SynExpr.NamedIndexedPropertySet(expr1 = e1; expr2 = e2) ->
                walkExpr e1
                walkExpr e2
            | SynExpr.DotNamedIndexedPropertySet(targetExpr = target; argExpr = e1; rhsExpr = e2) ->
                walkExpr target
                walkExpr e1
                walkExpr e2
            | SynExpr.TypeTest(expr = expr) -> walkExpr expr
            | SynExpr.Upcast(expr = expr) -> walkExpr expr
            | SynExpr.Downcast(expr = expr) -> walkExpr expr
            | SynExpr.InferredUpcast(expr = expr) -> walkExpr expr
            | SynExpr.InferredDowncast(expr = expr) -> walkExpr expr
            | SynExpr.AddressOf(expr = expr) -> walkExpr expr
            | SynExpr.JoinIn(lhsExpr = e1; rhsExpr = e2) ->
                walkExpr e1
                walkExpr e2
            | SynExpr.YieldOrReturn(expr = expr) -> walkExpr expr
            | SynExpr.YieldOrReturnFrom(expr = expr) -> walkExpr expr
            | SynExpr.DoBang(expr = expr) -> walkExpr expr
            | SynExpr.TraitCall(argExpr = expr) -> walkExpr expr
            | SynExpr.IndexFromEnd(expr = expr) -> walkExpr expr
            | SynExpr.IndexRange(expr1 = e1Opt; expr2 = e2Opt) ->
                match e1Opt with
                | Some e -> walkExpr e
                | None -> ()

                match e2Opt with
                | Some e -> walkExpr e
                | None -> ()
            | SynExpr.DebugPoint(innerExpr = expr) -> walkExpr expr
            | SynExpr.InterpolatedString(contents = parts) ->
                for part in parts do
                    match part with
                    | SynInterpolatedStringPart.FillExpr(fillExpr = expr) -> walkExpr expr
                    | SynInterpolatedStringPart.String _ -> ()
            | _ -> ()

    and walkBinding (binding: SynBinding) =
        match binding with
        | SynBinding(expr = body) -> walkExpr body

    let rec walkModuleDecl (decl: SynModuleDecl) =
        match decl with
        | SynModuleDecl.Let(bindings = bindings) ->
            for binding in bindings do
                walkBinding binding
        | SynModuleDecl.Expr(expr = expr) -> walkExpr expr
        | SynModuleDecl.NestedModule(decls = decls) ->
            for d in decls do
                walkModuleDecl d
        | SynModuleDecl.Types(typeDefns = typeDefs) ->
            for typeDef in typeDefs do
                walkTypeDef typeDef
        | _ -> ()

    and walkTypeDef (typeDef: SynTypeDefn) =
        match typeDef with
        | SynTypeDefn(members = members) ->
            for mem in members do
                walkMemberDefn mem

    and walkMemberDefn (mem: SynMemberDefn) =
        match mem with
        | SynMemberDefn.Member(memberDefn = binding) -> walkBinding binding
        | SynMemberDefn.LetBindings(bindings = bindings) ->
            for binding in bindings do
                walkBinding binding
        | SynMemberDefn.GetSetMember(memberDefnForGet = getBinding; memberDefnForSet = setBinding) ->
            match getBinding with
            | Some b -> walkBinding b
            | None -> ()

            match setBinding with
            | Some b -> walkBinding b
            | None -> ()
        | SynMemberDefn.AutoProperty(synExpr = expr) -> walkExpr expr
        | _ -> ()

    match parseTree with
    | ParsedInput.ImplFile(ParsedImplFileInput(contents = modules)) ->
        for moduleOrNs in modules do
            match moduleOrNs with
            | SynModuleOrNamespace(decls = decls) ->
                for decl in decls do
                    walkModuleDecl decl
    | ParsedInput.SigFile _ -> ()
