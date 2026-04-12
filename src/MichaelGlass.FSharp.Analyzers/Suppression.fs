module MichaelGlass.FSharp.Analyzers.Suppression

open FSharp.Compiler.Text

/// Check if any line in the range (or the line before the start) contains
/// the suppression comment pattern "{code}:ok".
let isLineSuppressed (sourceText: ISourceText) (range: range) (code: string) : bool =
    try
        let lineCount = sourceText.GetLineCount()
        let marker = code + ":ok"

        let checkLine idx =
            idx >= 0
            && idx < lineCount
            && sourceText.GetLineString(idx).Contains(marker)

        let startIdx = range.StartLine - 1
        let endIdx = range.EndLine - 1

        checkLine (startIdx - 1)
        || (seq { startIdx..endIdx } |> Seq.exists checkLine)
    with _ ->
        false
