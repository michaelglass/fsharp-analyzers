module MichaelGlass.FSharp.Analyzers.Tests.SuppressionTests

open Xunit
open Swensen.Unquote
open FSharp.Compiler.Text
open MichaelGlass.FSharp.Analyzers.Suppression

let private mkSource (text: string) = SourceText.ofString text

let private mkRange startLine startCol endLine endCol =
    Range.mkRange "test.fs" (Position.mkPos startLine startCol) (Position.mkPos endLine endCol)

/// The single-site question: is this one construct suppressed?
let private isSuppressed source range code = suppressor source code [ range ] range

[<Fact>]
let ``returns true when line at range contains code:ok`` () =
    let source = mkSource "let x = 1 // ABC001:ok"
    let range = mkRange 1 0 1 9
    test <@ isSuppressed source range "ABC001" = true @>

[<Fact>]
let ``returns false when line does not contain the code`` () =
    let source = mkSource "let x = 1"
    let range = mkRange 1 0 1 9
    test <@ isSuppressed source range "ABC001" = false @>

[<Fact>]
let ``checks the preceding line before range start`` () =
    let source = mkSource "// ABC001:ok\nlet x = 1"
    let range = mkRange 2 0 2 9
    test <@ isSuppressed source range "ABC001" = true @>

[<Fact>]
let ``checks all lines in a multi-line range`` () =
    let source = mkSource "let x =\n  1 +\n  2 // ABC001:ok"
    let range = mkRange 1 0 3 3
    test <@ isSuppressed source range "ABC001" = true @>

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
    test <@ isSuppressed source range "ABC001" = true @>

[<Fact>]
let ``binds a marker at the top of a longer justification block`` () =
    let source =
        mkSource "// ABC001:ok\n// reason, first line\n// reason, second line\n// reason, third line\nlet x = 1"

    let range = mkRange 5 0 5 9
    test <@ isSuppressed source range "ABC001" = true @>

[<Fact>]
let ``binds a marker on the last line of a justification block`` () =
    let source = mkSource "// reason, written first\n// ABC001:ok\nlet x = 1"
    let range = mkRange 3 0 3 9
    test <@ isSuppressed source range "ABC001" = true @>

[<Fact>]
let ``binds a marker in an indented comment block`` () =
    let source =
        mkSource "let f () =\n    // ABC001:ok\n    // reason\n    let x = 1\n    x"

    let range = mkRange 4 4 4 13
    test <@ isSuppressed source range "ABC001" = true @>

[<Fact>]
let ``does not bind a marker separated from the construct by a blank line`` () =
    let source = mkSource "// ABC001:ok\n// reason\n\nlet x = 1"
    let range = mkRange 4 0 4 9
    test <@ isSuppressed source range "ABC001" = false @>

[<Fact>]
let ``does not bind a marker separated from the construct by a line of code`` () =
    let source = mkSource "// ABC001:ok\n// reason\nlet y = 2\nlet x = 1"
    let range = mkRange 4 0 4 9
    test <@ isSuppressed source range "ABC001" = false @>

[<Fact>]
let ``does not bind a comment block carrying a different code`` () =
    let source = mkSource "// XYZ999:ok\n// reason\nlet x = 1"
    let range = mkRange 3 0 3 9
    test <@ isSuppressed source range "ABC001" = false @>

[<Fact>]
let ``does not bind a marker in the comment block below the construct`` () =
    let source = mkSource "let x = 1\n// ABC001:ok\n// reason\nlet y = 2"
    let range = mkRange 1 0 1 9
    test <@ isSuppressed source range "ABC001" = false @>

[<Fact>]
let ``stops the upward scan at the start of the file`` () =
    let source = mkSource "// reason\n// more reason\nlet x = 1"
    let range = mkRange 3 0 3 9
    test <@ isSuppressed source range "ABC001" = false @>

// --- One marker, one site ------------------------------------------------------
// A marker binds the construct it is written for, never one that only ENCLOSES
// it. Before this rule a marker on any line of a range bound that range, so a
// marker in a nested construct also silenced every construct around it.

// line 1  outer =
// line 2      inner =
// line 3          work
// line 4          // ABC001:ok
// line 5      after
let private nestedWithMarkerInInner =
    mkSource "outer =\n    inner =\n        work\n        // ABC001:ok\n    after"

let private outerSite = mkRange 1 0 5 9
let private innerSite = mkRange 2 4 4 20

[<Fact>]
let ``a marker inside a nested site binds the nested site`` () =
    let isSuppressed =
        suppressor nestedWithMarkerInInner "ABC001" [ outerSite; innerSite ]

    test <@ isSuppressed innerSite @>

[<Fact>]
let ``a marker inside a nested site does not bind the site enclosing it`` () =
    let isSuppressed =
        suppressor nestedWithMarkerInInner "ABC001" [ outerSite; innerSite ]

    test <@ not (isSuppressed outerSite) @>

[<Fact>]
let ``a marker above a nested site binds it and not the enclosing site`` () =
    // line 1 outer =   line 2 // ABC001:ok   line 3 inner (to line 4)   line 5 after
    let source =
        mkSource "outer =\n    // ABC001:ok\n    inner =\n        work\n    after"

    let outer = mkRange 1 0 5 9
    let inner = mkRange 3 4 4 12
    let isSuppressed = suppressor source "ABC001" [ outer; inner ]

    test <@ isSuppressed inner @>
    test <@ not (isSuppressed outer) @>

[<Fact>]
let ``a marker on the enclosing site's first line binds it and not the site starting below`` () =
    let source = mkSource "outer = // ABC001:ok\n    inner =\n        work\n    after"
    let outer = mkRange 1 0 4 9
    let inner = mkRange 2 4 3 12
    let isSuppressed = suppressor source "ABC001" [ outer; inner ]

    test <@ isSuppressed outer @>
    test <@ not (isSuppressed inner) @>

[<Fact>]
let ``a marker in the enclosing site after the nested one ends binds the enclosing site`` () =
    let source =
        mkSource "outer =\n    inner =\n        work\n    // ABC001:ok\n    after"

    let outer = mkRange 1 0 5 9
    let inner = mkRange 2 4 3 12
    let isSuppressed = suppressor source "ABC001" [ outer; inner ]

    test <@ isSuppressed outer @>
    test <@ not (isSuppressed inner) @>

[<Fact>]
let ``a trailing marker binds its own line's site and not the site on the next line`` () =
    let source = mkSource "let a = sql1 // ABC001:ok\nlet b = sql2"
    let first = mkRange 1 8 1 12
    let second = mkRange 2 8 2 12
    let isSuppressed = suppressor source "ABC001" [ first; second ]

    test <@ isSuppressed first @>
    test <@ not (isSuppressed second) @>

[<Fact>]
let ``sites no placement can tell apart are bound together`` () =
    let source = mkSource "// ABC001:ok\nlet a = sql1, sql2"
    let first = mkRange 2 8 2 12
    let second = mkRange 2 14 2 18
    let isSuppressed = suppressor source "ABC001" [ first; second ]

    test <@ isSuppressed first @>
    test <@ isSuppressed second @>

[<Fact>]
let ``a site absent from the site list is never suppressed`` () =
    let source = mkSource "// ABC001:ok\nlet x = 1"
    let listed = mkRange 2 0 2 9
    let unlisted = mkRange 2 4 2 5
    let isSuppressed = suppressor source "ABC001" [ listed ]

    test <@ not (isSuppressed unlisted) @>
