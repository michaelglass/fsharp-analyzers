/// <summary>
/// Checks inline suppression comments that silence specific analyzer diagnostics.
/// A diagnostic is suppressed when the marker <c>// {CODE}:ok</c> appears on any line
/// of the diagnostic's own range, on the line immediately above it, or anywhere in the
/// contiguous <c>//</c> comment block directly above it.
/// </summary>
module MichaelGlass.FSharp.Analyzers.Suppression

open System
open FSharp.Compiler.Text

/// <summary>Returns true when the line is a whole-line <c>//</c> comment.</summary>
let private isCommentLine (line: string) =
    line.TrimStart().StartsWith("//", StringComparison.Ordinal)

/// <summary>
/// Checks whether a diagnostic at the given range is suppressed by an inline comment.
/// Looks for <c>{code}:ok</c> on any line within the range, on the line immediately
/// before it, or on any line of the contiguous comment block above it — so a marker
/// written above its own multi-line justification still binds.
/// The comment block ends at the first blank line or line of code, which is what keeps
/// a marker that has drifted away from its construct from suppressing something else.
/// </summary>
/// <param name="sourceText">The source text of the file being analyzed.</param>
/// <param name="range">The range of the diagnostic to check.</param>
/// <param name="code">The analyzer diagnostic code (e.g. "MGA-WILDCARD-001").</param>
/// <returns>True if a suppression comment is found.</returns>
let isLineSuppressed (sourceText: ISourceText) (range: range) (code: string) : bool =
    try
        let lineCount = sourceText.GetLineCount()
        let marker = code + ":ok"

        let tryLine idx =
            if idx >= 0 && idx < lineCount then
                Some(sourceText.GetLineString idx)
            else
                None

        let checkLine idx =
            tryLine idx |> Option.exists (fun line -> line.Contains marker)

        let startIdx = range.StartLine - 1
        let endIdx = range.EndLine - 1

        // Line indices of the contiguous comment block immediately above the range,
        // walking upwards; stops at the first blank line, line of code, or file start.
        let precedingCommentBlock =
            Seq.initInfinite (fun offset -> startIdx - 1 - offset)
            |> Seq.takeWhile (fun idx -> tryLine idx |> Option.exists isCommentLine)

        checkLine (startIdx - 1)
        || Seq.exists checkLine precedingCommentBlock
        || (seq { startIdx..endIdx } |> Seq.exists checkLine)
    with _ ->
        false
