module MichaelGlass.FSharp.Analyzers.Tests.EditorConfigTests

open Xunit
open Swensen.Unquote
open MichaelGlass.FSharp.Analyzers.EditorConfig

let private sampleFile =
    System.IO.Path.Combine [| __SOURCE_DIRECTORY__; "data"; "editorconfig-test"; "Sample.fs" |]

[<Fact>]
let ``getListProperty returns configured comma-separated values`` () =
    let result = getListProperty sampleFile "my_list_key"
    test <@ result = [ "alpha"; "bravo"; "charlie" ] @>

[<Fact>]
let ``getListProperty returns empty list for missing key`` () =
    let result = getListProperty sampleFile "nonexistent_key"
    test <@ List.isEmpty result @>

[<Fact>]
let ``getProperty returns Some for present key`` () =
    let result = getProperty sampleFile "my_single_key"
    test <@ result = Some "hello" @>

[<Fact>]
let ``getProperty returns None for missing key`` () =
    let result = getProperty sampleFile "nonexistent_key"
    test <@ result = None @>
