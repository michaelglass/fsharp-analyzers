/// Analyzer that flags raw SQL strings in source files.
/// Prefer type-safe query builders instead of raw SQL strings.
/// Code: MGA-RAWSQL-001
module MichaelGlass.FSharp.Analyzers.RawSqlAnalyzer

open System
open FSharp.Analyzers.SDK
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text

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

let private looksLikeSql (s: string) =
    let trimmed = s.TrimStart()

    sqlKeywords
    |> Array.exists (fun kw -> trimmed.StartsWith(kw, StringComparison.OrdinalIgnoreCase))

let private isFileExcluded (fileName: string) =
    let excludedFiles =
        EditorConfig.getListProperty fileName "mga_rawsql_excluded_files"

    if List.isEmpty excludedFiles then
        false
    else
        let currentFile = System.IO.Path.GetFileName(fileName)
        List.contains currentFile excludedFiles

[<CliAnalyzer("RawSqlAnalyzer",
              "Flags raw SQL strings in source files. Prefer type-safe query builders instead of raw SQL.")>]
let rawSqlAnalyzer: Analyzer<CliContext> =
    fun (context: CliContext) ->
        async {
            if isFileExcluded context.FileName then
                return []
            else
                let ranges = ResizeArray<range>()

                AstWalk.walkParseTree
                    (fun expr ->
                        match expr with
                        | SynExpr.Const(SynConst.String(text = text), range) when looksLikeSql text ->
                            ranges.Add(range)
                            true
                        | SynExpr.InterpolatedString(contents = parts; range = range) ->
                            let firstText =
                                parts
                                |> List.tryPick (fun part ->
                                    match part with
                                    | SynInterpolatedStringPart.String(text, _) when text.Length > 0 -> Some text
                                    | _ -> None)

                            match firstText with
                            | Some text when looksLikeSql text -> ranges.Add(range)
                            | _ -> ()

                            true
                        | _ -> true)
                    context.ParseFileResults.ParseTree

                let ranges = ranges |> Seq.toList

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
