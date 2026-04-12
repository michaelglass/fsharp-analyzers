/// <summary>
/// Flags catch-all wildcard patterns (<c>| _ -></c>) on discriminated union match expressions.
/// Wildcards on DUs hide exhaustiveness checking — when a new case is added, the compiler
/// won't warn about unhandled cases. Common types like option and Result are allowed by default.
/// </summary>
/// <remarks>
/// <para>Code: <c>MGA-WILDCARD-001</c></para>
/// <para>Configure allowed types via <c>mga_wildcard_allowed_types</c> in .editorconfig.</para>
/// <para>Suppress with <c>// MGA-WILDCARD-001:ok</c>.</para>
/// </remarks>
module MichaelGlass.FSharp.Analyzers.WildcardAnalyzer

open FSharp.Analyzers.SDK
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text

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

let private getAllowedTypes (fileName: string) =
    let extra = EditorConfig.getListProperty fileName "mga_wildcard_allowed_types"

    if List.isEmpty extra then
        defaultAllowedTypes
    else
        Set.union defaultAllowedTypes (Set.ofList extra)

/// <summary>Strips parentheses from a pattern, returning the inner pattern.</summary>
let rec private unwrapParen (pat: SynPat) =
    match pat with
    | SynPat.Paren(inner, _) -> unwrapParen inner
    | _ -> pat

/// <summary>Returns true if the pattern (after unwrapping parens) is a wildcard.</summary>
let private isWildcard (pat: SynPat) =
    match unwrapParen pat with
    | SynPat.Wild _ -> true
    | _ -> false

/// <summary>Returns true if the pattern (after unwrapping parens) is a long identifier (union case).</summary>
let private isLongIdent (pat: SynPat) =
    match unwrapParen pat with
    | SynPat.LongIdent _ -> true
    | _ -> false

/// <summary>Gets the range of a wildcard pattern, or None if not a wildcard.</summary>
let private getWildcardRange (pat: SynPat) =
    match unwrapParen pat with
    | SynPat.Wild range -> Some range
    | _ -> None

/// <summary>Extracts the pattern from a match clause.</summary>
let private getClausePattern (clause: SynMatchClause) =
    match clause with
    | SynMatchClause(pat = pat) -> pat

/// <summary>Extracts the body expression from a match clause.</summary>
let private getClauseBody (clause: SynMatchClause) =
    match clause with
    | SynMatchClause(resultExpr = body) -> body

/// <summary>Returns true if any clause in the list matches on a long identifier (union case).</summary>
let private hasLongIdentSibling (clauses: SynMatchClause list) =
    clauses |> List.exists (fun c -> isLongIdent (getClausePattern c))

/// <summary>Gets the first long identifier from a list of match clauses, if any.</summary>
let private getFirstLongIdentInfo (clauses: SynMatchClause list) : SynLongIdent option =
    clauses
    |> List.tryPick (fun c ->
        match unwrapParen (getClausePattern c) with
        | SynPat.LongIdent(longDotId = synLongIdent) -> Some synLongIdent
        | _ -> None)

/// <summary>
/// Resolves the discriminated union type from a match clause's long identifier
/// using the typed check results. Falls back to AccessPath + CompiledName
/// for generic type abbreviations (e.g. option).
/// </summary>
/// <param name="checkResults">Typed check results for symbol resolution.</param>
/// <param name="sourceText">Source text for line lookups.</param>
/// <param name="synLongIdent">The long identifier from a union case pattern.</param>
/// <returns>The full type name, or None if not a union case.</returns>
let private tryResolveUnionType
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
                    try
                        Some(typeDef.AccessPath + "." + typeDef.CompiledName)
                    with _ ->
                        None
            | _ -> None
        | None -> None
    with _ ->
        None

/// <summary>
/// Returns true when the match expression targets a resolved F# DU that is NOT
/// in the allowlist. Unresolvable types (enums, .NET types) are not flagged.
/// </summary>
let private shouldFlagWildcard
    (checkResults: FSharpCheckFileResults)
    (sourceText: ISourceText)
    (allowedTypes: Set<string>)
    (clauses: SynMatchClause list)
    : bool =
    match getFirstLongIdentInfo clauses with
    | Some synLongIdent ->
        match tryResolveUnionType checkResults sourceText synLongIdent with
        | Some fullName -> not (Set.contains fullName allowedTypes)
        | None -> false
    | None -> false

let private findProblematicWildcards
    (checkResults: FSharpCheckFileResults)
    (sourceText: ISourceText)
    (allowedTypes: Set<string>)
    (clauses: SynMatchClause list)
    : range list =
    if not (hasLongIdentSibling clauses) then
        []
    elif not (shouldFlagWildcard checkResults sourceText allowedTypes clauses) then
        []
    else
        clauses
        |> List.choose (fun c ->
            let pat = getClausePattern c
            if isWildcard pat then getWildcardRange pat else None)

/// <summary>
/// CLI analyzer entry point. Walks all match expressions and flags wildcard
/// patterns on non-allowed discriminated union types.
/// </summary>
[<CliAnalyzer("WildcardOnDUAnalyzer", "Forbids catch-all wildcards on DU match expressions")>]
let wildcardAnalyzer: Analyzer<CliContext> =
    fun (context: CliContext) ->
        async {
            let allowedTypes = getAllowedTypes context.FileName
            let checkResults = context.CheckFileResults
            let sourceText = context.SourceText
            let ranges = ResizeArray<range>()

            AstWalk.walkParseTree
                (fun expr ->
                    match expr with
                    | SynExpr.Match(clauses = clauses)
                    | SynExpr.MatchBang(clauses = clauses)
                    | SynExpr.MatchLambda(matchClauses = clauses) ->
                        let problematic =
                            findProblematicWildcards checkResults sourceText allowedTypes clauses

                        ranges.AddRange(problematic)
                        true
                    | _ -> true)
                context.ParseFileResults.ParseTree

            let wildcardRanges = ranges |> Seq.toList

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
