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
