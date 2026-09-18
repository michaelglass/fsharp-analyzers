module MichaelGlass.FSharp.Analyzers.Tests.EditorConfigTests

open System.IO
open Xunit
open Swensen.Unquote
open MichaelGlass.FSharp.Analyzers.EditorConfig
open MichaelGlass.FSharp.Analyzers.Tests.Common

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

// --- Configuration that changes under a long-lived process -------------------------
// An analyzer host is a daemon: it loads this assembly once and keeps it for hours,
// analyzing the same paths again and again. A reader that answers from its first
// parse forever reports the OLD configuration to every later run, and the host stores
// that answer as the verdict for the NEW configuration. "Nothing found" then means
// "nothing was looked for", which is the failure this whole area guards against.

[<Fact>]
let ``getListProperty reflects an edit to the editorconfig it read from`` () =
    let dir = newConfiguredTree ()
    let file = Path.Combine(dir, "Sample.fs")
    File.WriteAllText(file, "module Sample\n")

    // The rewritten value is deliberately a DIFFERENT LENGTH from the first. This
    // module's own caching ignores both size and timestamp, so the test is red
    // against the old code either way — but EditorConfig.Core keys its internal file
    // cache on size and last-write time, and a same-length rewrite would then rest on
    // the filesystem's timestamp granularity to be noticed. A test whose subject is
    // stale configuration should not itself depend on a stat being fine-grained.
    writeEditorConfig dir "my_list_key = alpha"
    test <@ getListProperty file "my_list_key" = [ "alpha" ] @>

    writeEditorConfig dir "my_list_key = bravo, charlie"
    test <@ getListProperty file "my_list_key" = [ "bravo"; "charlie" ] @>

[<Fact>]
let ``getProperty reports a key the editorconfig gained after the first read`` () =
    let dir = newConfiguredTree ()
    let file = Path.Combine(dir, "Sample.fs")
    File.WriteAllText(file, "module Sample\n")

    writeEditorConfig dir "my_single_key = hello"
    test <@ getProperty file "later_key" = None @>

    writeEditorConfig dir "my_single_key = hello\nlater_key = world"
    test <@ getProperty file "later_key" = Some "world" @>

[<Fact>]
let ``a sibling file in the same directory sees the edit too`` () =
    // The chain is resolved per DIRECTORY, not per file, so a reader that holds one
    // could answer correctly for the file that triggered the re-read and stale for
    // every other file beside it. Read one file first, edit, then ask its neighbour.
    let dir = newConfiguredTree ()
    let first = Path.Combine(dir, "First.fs")
    let second = Path.Combine(dir, "Second.fs")
    File.WriteAllText(first, "module First\n")
    File.WriteAllText(second, "module Second\n")

    writeEditorConfig dir "my_list_key = alpha"
    test <@ getListProperty first "my_list_key" = [ "alpha" ] @>

    writeEditorConfig dir "my_list_key = bravo, charlie"
    test <@ getListProperty second "my_list_key" = [ "bravo"; "charlie" ] @>

[<Fact>]
let ``a lookup for an unusable path degrades to None rather than throwing`` () =
    // The documented contract, now that every lookup reads from disk: a per-file
    // failure is answered with "no key", never thrown into the analyzer that asked.
    // A parser-CONSTRUCTION failure is a different thing and still propagates.
    test <@ getProperty "" "my_single_key" = None @>
    test <@ List.isEmpty (getListProperty "" "my_list_key") @>

[<Fact>]
let ``a config file appearing and disappearing mid-chain changes the answer`` () =
    // Effective configuration is a CHAIN, and it can change without a single surviving
    // byte changing: add an .editorconfig between the file and the root, or delete one,
    // and the analysed file's bytes and the root config's bytes are both untouched.
    // Anything that remembers the files it happened to read first misses both events.
    let dir = newConfiguredTree ()
    let nested = Path.Combine(dir, "nested")
    Directory.CreateDirectory nested |> ignore
    let file = Path.Combine(nested, "Sample.fs")
    File.WriteAllText(file, "module Sample\n")

    writeEditorConfig dir "my_list_key = alpha"
    test <@ getListProperty file "my_list_key" = [ "alpha" ] @>

    // A nearer config APPEARS. Nothing that already existed changed.
    writeNestedEditorConfig nested "my_list_key = bravo, charlie"
    test <@ getListProperty file "my_list_key" = [ "bravo"; "charlie" ] @>

    // ...and DISAPPEARS again, restoring the root's value.
    File.Delete(Path.Combine(nested, ".editorconfig"))
    test <@ getListProperty file "my_list_key" = [ "alpha" ] @>
