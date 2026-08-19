module MichaelGlass.FSharp.Analyzers.Tests.SuppressionTests

open Xunit
open Swensen.Unquote
open FSharp.Compiler.Text
open MichaelGlass.FSharp.Analyzers.Suppression

let private mkSource (text: string) = SourceText.ofString text

let private mkRange startLine startCol endLine endCol =
    Range.mkRange "test.fs" (Position.mkPos startLine startCol) (Position.mkPos endLine endCol)

[<Fact>]
let ``returns true when line at range contains code:ok`` () =
    let source = mkSource "let x = 1 // ABC001:ok"
    let range = mkRange 1 0 1 9
    test <@ isLineSuppressed source range "ABC001" = true @>

[<Fact>]
let ``returns false when line does not contain the code`` () =
    let source = mkSource "let x = 1"
    let range = mkRange 1 0 1 9
    test <@ isLineSuppressed source range "ABC001" = false @>

[<Fact>]
let ``checks the preceding line before range start`` () =
    let source = mkSource "// ABC001:ok\nlet x = 1"
    let range = mkRange 2 0 2 9
    test <@ isLineSuppressed source range "ABC001" = true @>

[<Fact>]
let ``checks all lines in a multi-line range`` () =
    let source = mkSource "let x =\n  1 +\n  2 // ABC001:ok"
    let range = mkRange 1 0 3 3
    test <@ isLineSuppressed source range "ABC001" = true @>

// --- Placement within the comment block above the construct -------------------
// The marker binds anywhere in the contiguous `//` block immediately above the
// construct. The block ends at the first blank line or line of code, so a marker
// that drifted away from what it once suppressed still stops binding.

[<Fact>]
let ``binds a marker written above its own justification`` () =
    let source =
        mkSource
            "// ABC001:ok - a part we cannot decode contributes no fields;\n// the caller's report is still recorded from whatever else parsed.\nlet x = 1"

    let range = mkRange 3 0 3 9
    test <@ isLineSuppressed source range "ABC001" = true @>

[<Fact>]
let ``binds a marker at the top of a longer justification block`` () =
    let source =
        mkSource "// ABC001:ok\n// reason, first line\n// reason, second line\n// reason, third line\nlet x = 1"

    let range = mkRange 5 0 5 9
    test <@ isLineSuppressed source range "ABC001" = true @>

[<Fact>]
let ``binds a marker on the last line of a justification block`` () =
    let source = mkSource "// reason, written first\n// ABC001:ok\nlet x = 1"
    let range = mkRange 3 0 3 9
    test <@ isLineSuppressed source range "ABC001" = true @>

[<Fact>]
let ``binds a marker in an indented comment block`` () =
    let source =
        mkSource "let f () =\n    // ABC001:ok\n    // reason\n    let x = 1\n    x"

    let range = mkRange 4 4 4 13
    test <@ isLineSuppressed source range "ABC001" = true @>

[<Fact>]
let ``does not bind a marker separated from the construct by a blank line`` () =
    let source = mkSource "// ABC001:ok\n// reason\n\nlet x = 1"
    let range = mkRange 4 0 4 9
    test <@ isLineSuppressed source range "ABC001" = false @>

[<Fact>]
let ``does not bind a marker separated from the construct by a line of code`` () =
    let source = mkSource "// ABC001:ok\n// reason\nlet y = 2\nlet x = 1"
    let range = mkRange 4 0 4 9
    test <@ isLineSuppressed source range "ABC001" = false @>

[<Fact>]
let ``does not bind a comment block carrying a different code`` () =
    let source = mkSource "// XYZ999:ok\n// reason\nlet x = 1"
    let range = mkRange 3 0 3 9
    test <@ isLineSuppressed source range "ABC001" = false @>

[<Fact>]
let ``does not bind a marker in the comment block below the construct`` () =
    let source = mkSource "let x = 1\n// ABC001:ok\n// reason\nlet y = 2"
    let range = mkRange 1 0 1 9
    test <@ isLineSuppressed source range "ABC001" = false @>

[<Fact>]
let ``stops the upward scan at the start of the file`` () =
    let source = mkSource "// reason\n// more reason\nlet x = 1"
    let range = mkRange 3 0 3 9
    test <@ isLineSuppressed source range "ABC001" = false @>
