/// <summary>
/// Flags raw SQL strings in source code. Raw SQL is vulnerable to injection,
/// hard to refactor, and invisible to the type system. Prefer type-safe query
/// builders or at minimum a dedicated SQL module.
/// </summary>
/// <remarks>
/// <para>Code: <c>MGA-RAWSQL-001</c></para>
/// <para>Exclude files (e.g. migrations) via <c>mga_rawsql_excluded_files</c> in .editorconfig.</para>
/// <para>Suppress with <c>// MGA-RAWSQL-001:ok</c>.</para>
/// </remarks>
module MichaelGlass.FSharp.Analyzers.RawSqlAnalyzer

open FSharp.Analyzers.SDK
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text

/// <summary>
/// Leading statement keywords. Matched <em>case-sensitively against upper-case</em>:
/// real raw SQL conventionally upper-cases its keywords (<c>SELECT 1</c>,
/// <c>WITH ins AS (…)</c>), whereas English prose that happens to start with the same
/// word is Title- or lower-cased (<c>"Create account"</c>, <c>"Update Topic"</c>).
/// A case-insensitive match flagged that prose as raw SQL.
/// </summary>
let private sqlKeywords =
    [| "SELECT"
       "INSERT"
       "UPDATE"
       "DELETE"
       "WITH"
       "CREATE"
       "ALTER"
       "DROP"
       "TRUNCATE" |]

let private boundaryChars = [| ' '; '\t'; '\r'; '\n'; '('; ')'; ','; ';' |]

let private looksLikeSql (s: string) =
    // TrimStart only — trailing content (e.g. the space in a "SELECT " fragment being
    // concatenated) is what distinguishes a statement from a bare keyword label.
    let started = s.TrimStart([| ' '; '\t'; '\r'; '\n'; '('; '"'; '\''; '`' |])

    let word =
        match started.IndexOfAny(boundaryChars) with
        | -1 -> started
        | i -> started.Substring(0, i)

    // Case-sensitive: only an upper-cased keyword counts (see sqlKeywords remark).
    // The keyword must also begin a statement, not BE the whole string: a bare "DELETE"
    // (an HTTP method / label) is not raw SQL, whereas a "DELETE FROM …" statement or a
    // "SELECT " fragment being concatenated is.
    (sqlKeywords |> Array.exists (fun kw -> word = kw))
    && started.Length > word.Length

let private isFileExcluded (fileName: string) =
    let excludedFiles =
        EditorConfig.getListProperty fileName "mga_rawsql_excluded_files"

    let currentFile = System.IO.Path.GetFileName(fileName)
    List.contains currentFile excludedFiles

/// <summary>
/// CLI analyzer entry point. Scans string constants and interpolated strings
/// for SQL keywords (SELECT, INSERT, UPDATE, DELETE, etc.).
/// </summary>
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
