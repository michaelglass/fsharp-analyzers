/// <summary>
/// Checks inline suppression comments that silence specific analyzer diagnostics.
/// A diagnostic is suppressed when any line in its range (or the line immediately before)
/// contains the comment pattern <c>// {CODE}:ok</c>.
/// </summary>
module MichaelGlass.FSharp.Analyzers.Suppression

open FSharp.Compiler.Text

/// <summary>
/// Checks whether a diagnostic at the given range is suppressed by an inline comment.
/// Looks for <c>{code}:ok</c> on any line within the range or the line immediately before.
/// </summary>
/// <param name="sourceText">The source text of the file being analyzed.</param>
/// <param name="range">The range of the diagnostic to check.</param>
/// <param name="code">The analyzer diagnostic code (e.g. "MGA-WILDCARD-001").</param>
/// <returns>True if a suppression comment is found.</returns>
let isLineSuppressed (sourceText: ISourceText) (range: range) (code: string) : bool =
    try
        let lineCount = sourceText.GetLineCount()
        let marker = code + ":ok"

        let checkLine idx =
            idx >= 0 && idx < lineCount && sourceText.GetLineString(idx).Contains(marker)

        let startIdx = range.StartLine - 1
        let endIdx = range.EndLine - 1

        checkLine (startIdx - 1) || (seq { startIdx..endIdx } |> Seq.exists checkLine)
    with _ ->
        false
