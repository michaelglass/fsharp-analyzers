module MichaelGlass.FSharp.Analyzers.Tests.Common

open FSharp.Analyzers.SDK.Testing

let projectOptions =
    mkOptionsFromProject "net10.0" [] |> Async.AwaitTask |> Async.RunSynchronously

let getContextForSource (source: string) = getContext projectOptions source

let readTestData (parts: string list) =
    let path =
        System.IO.Path.Combine [| yield __SOURCE_DIRECTORY__; yield "data"; yield! parts |]

    System.IO.File.ReadAllText path

/// <summary>
/// Creates a fresh temporary directory owning its own root <c>.editorconfig</c>, so a
/// test can change analyzer configuration underneath a file without touching the
/// repository's own config chain. <c>root = true</c> stops the walk-up, which keeps
/// the test independent of whatever sits above the system temp directory.
/// </summary>
let newConfiguredTree () =
    let name = System.Guid.NewGuid().ToString("N")

    let dir =
        System.IO.Path.Combine(System.IO.Path.GetTempPath(), $"mga-editorconfig-%s{name}")

    System.IO.Directory.CreateDirectory dir |> ignore
    dir

/// <summary>
/// Writes (or rewrites) the <c>[*.fs]</c> section of a tree's <c>.editorconfig</c>.
/// </summary>
let writeEditorConfig (dir: string) (properties: string) =
    let contents = $"root = true\n\n[*.fs]\n%s{properties}\n"
    System.IO.File.WriteAllText(System.IO.Path.Combine(dir, ".editorconfig"), contents)

/// <summary>
/// Writes an <c>.editorconfig</c> that does NOT stop the walk-up, for a file part-way
/// along a chain rather than at its root.
/// </summary>
let writeNestedEditorConfig (dir: string) (properties: string) =
    let contents = $"[*.fs]\n%s{properties}\n"
    System.IO.File.WriteAllText(System.IO.Path.Combine(dir, ".editorconfig"), contents)
