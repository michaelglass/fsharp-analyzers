/// Analyzer that forbids catch-all wildcards on discriminated union types.
/// Code: MGA-WILDCARD-001
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
    Set.union defaultAllowedTypes (Set.ofList extra)

let rec unwrapParen (pat: SynPat) =
    match pat with
    | SynPat.Paren(inner, _) -> unwrapParen inner
    | _ -> pat

let isWildcard (pat: SynPat) =
    match unwrapParen pat with
    | SynPat.Wild _ -> true
    | _ -> false

let isLongIdent (pat: SynPat) =
    match unwrapParen pat with
    | SynPat.LongIdent _ -> true
    | _ -> false

let getWildcardRange (pat: SynPat) =
    match unwrapParen pat with
    | SynPat.Wild range -> Some range
    | _ -> None

let getClausePattern (clause: SynMatchClause) =
    match clause with
    | SynMatchClause(pat = pat) -> pat

let getClauseBody (clause: SynMatchClause) =
    match clause with
    | SynMatchClause(resultExpr = body) -> body

let hasLongIdentSibling (clauses: SynMatchClause list) =
    clauses |> List.exists (fun c -> isLongIdent (getClausePattern c))

let getFirstLongIdentInfo (clauses: SynMatchClause list) : SynLongIdent option =
    clauses
    |> List.tryPick (fun c ->
        match unwrapParen (getClausePattern c) with
        | SynPat.LongIdent(longDotId = synLongIdent) -> Some synLongIdent
        | _ -> None)

/// Try to resolve the union type from a SynLongIdent using typed check results.
/// Generic type abbreviations (e.g. option<'T>) don't have TryFullName,
/// so we fall back to AccessPath + CompiledName.
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
                    try
                        Some(typeDef.AccessPath + "." + typeDef.CompiledName)
                    with _ ->
                        None
            | _ -> None
        | None -> None
    with _ ->
        None

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

[<CliAnalyzer("WildcardOnDUAnalyzer",
              "Forbids catch-all wildcards on DU match expressions")>]
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
                        let problematic = findProblematicWildcards checkResults sourceText allowedTypes clauses
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
