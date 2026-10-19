module MichaelGlass.FSharp.Analyzers.Tests.AstWalkTests

open Xunit
open Swensen.Unquote
open MichaelGlass.FSharp.Analyzers.Tests.Common
open MichaelGlass.FSharp.Analyzers.RawSqlAnalyzer

/// Runs MGA-RAWSQL-001 over <paramref name="source"/> and returns the flagged lines, so each
/// test asks one question of the walker: did it reach the string on the marked line?
let private flaggedRawSql (source: string) =
    rawSqlAnalyzer (getContextForSource source)
    |> Async.RunSynchronously
    |> List.filter (fun m -> m.Code = "MGA-RAWSQL-001")
    |> flaggedLines

let private assertFlagsMarkedLines (source: string) =
    let expected = linesMarkedWith "// flag" source
    test <@ not (List.isEmpty expected) @>
    test <@ flaggedRawSql source = expected @>

[<Fact>]
let ``positive control: reaches a module-level let binding`` () =
    assertFlagsMarkedLines
        """module TestData.ModuleLet

let query = "SELECT * FROM users" // flag
"""

[<Fact>]
let ``positive control: reaches an augmentation member`` () =
    assertFlagsMarkedLines
        """module TestData.Augmentation

type R =
    { Id: int }

    member _.Query = "SELECT * FROM users" // flag
"""

[<Fact>]
let ``reaches a class member`` () =
    assertFlagsMarkedLines
        """module TestData.ClassMember

type Repo() =
    member _.Load() = "SELECT * FROM users" // flag
"""

[<Fact>]
let ``reaches a class let binding`` () =
    assertFlagsMarkedLines
        """module TestData.ClassLet

type Repo() =
    let query = "SELECT * FROM users" // flag
    member _.Query = query
"""

[<Fact>]
let ``reaches a class auto property and get-set property`` () =
    assertFlagsMarkedLines
        """module TestData.ClassProperties

type Repo() =
    let mutable q = ""
    member val Auto = "SELECT * FROM users" with get, set // flag

    member _.Prop
        with get () = "SELECT * FROM orders" // flag
        and set (v: string) = q <- v
"""

[<Fact>]
let ``reaches an interface implementation inside a class`` () =
    assertFlagsMarkedLines
        """module TestData.InterfaceImpl

type IRepo =
    abstract Load: unit -> string

type Repo() =
    interface IRepo with
        member _.Load() = "SELECT * FROM users" // flag
"""

[<Fact>]
let ``reaches a class nested in a module, exactly once`` () =
    assertFlagsMarkedLines
        """module TestData.NestedClass

module Inner =
    type Repo() =
        member _.Load() = "SELECT * FROM users" // flag
"""
