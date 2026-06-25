/// <summary>
/// Regression tests for the NuGet package layout.
///
/// The analyzer is consumed inside an analyzer HOST (the fshw daemon) where only the
/// bundled DLLs are on the load path — NuGet does NOT resolve the package's transitive
/// dependencies there. So EditorConfig.Core's transitive closure must be bundled into
/// the analyzers/dotnet/fs folder, or `EditorConfigParser()` throws at construction and
/// every `.editorconfig` MGA key silently falls back to its default.
///
/// A normal unit test can't catch this: the test project resolves those deps via NuGet,
/// so the bug only bites the bundled/host context. These tests therefore inspect the
/// actual produced .nupkg.
/// </summary>
module MichaelGlass.FSharp.Analyzers.Tests.PackagingTests

open System
open System.Diagnostics
open System.IO
open System.IO.Compression
open Xunit
open Swensen.Unquote

let private repoRoot =
    // tests/MichaelGlass.FSharp.Analyzers.Tests/<this file> -> repo root is two levels up.
    let dir = DirectoryInfo(__SOURCE_DIRECTORY__).Parent.Parent
    dir.FullName

let private analyzerProject =
    Path.Combine(repoRoot, "src", "MichaelGlass.FSharp.Analyzers", "MichaelGlass.FSharp.Analyzers.fsproj")

/// <summary>Packs the analyzer (reusing the already-built binaries) and returns the .nupkg path.</summary>
let private packAnalyzer () =
    let outDir =
        Path.Combine(Path.GetTempPath(), "mga-pack-" + Guid.NewGuid().ToString("N"))

    Directory.CreateDirectory(outDir) |> ignore

    // No --no-build / --no-restore: pack is self-sufficient so the test does not depend
    // on a particular configuration having been built first (clean-CI safe).
    let psi = ProcessStartInfo("dotnet")
    psi.ArgumentList.Add("pack")
    psi.ArgumentList.Add(analyzerProject)
    psi.ArgumentList.Add("-c")
    psi.ArgumentList.Add("Release")
    psi.ArgumentList.Add("-o")
    psi.ArgumentList.Add(outDir)
    psi.RedirectStandardOutput <- true
    psi.RedirectStandardError <- true
    psi.WorkingDirectory <- repoRoot

    use proc = Process.Start(psi)
    let stdout = proc.StandardOutput.ReadToEnd()
    let stderr = proc.StandardError.ReadToEnd()
    proc.WaitForExit()

    if proc.ExitCode <> 0 then
        failwithf "dotnet pack failed (exit %d).\nSTDOUT:\n%s\nSTDERR:\n%s" proc.ExitCode stdout stderr

    match Directory.GetFiles(outDir, "*.nupkg") with
    | [||] -> failwithf "no .nupkg produced in %s.\nSTDOUT:\n%s" outDir stdout
    | files -> Array.head files

/// <summary>Names of the DLLs packed under analyzers/dotnet/fs/.</summary>
let private analyzerPathDlls () =
    let nupkg = packAnalyzer ()
    use archive = ZipFile.OpenRead(nupkg)

    archive.Entries
    |> Seq.map (fun e -> e.FullName)
    |> Seq.filter (fun n -> n.StartsWith("analyzers/dotnet/fs/") && n.EndsWith(".dll"))
    |> Seq.map Path.GetFileName
    |> Set.ofSeq

[<Fact>]
let ``package bundles EditorConfig.Core's transitive dependency closure`` () =
    let dlls = analyzerPathDlls ()

    // EditorConfig.Core itself plus every transitive runtime dep it loads at construction.
    let required =
        [ "EditorConfig.Core.dll"
          "System.IO.Abstractions.dll"
          "TestableIO.System.IO.Abstractions.dll"
          "TestableIO.System.IO.Abstractions.Wrappers.dll"
          "Testably.Abstractions.FileSystem.Interface.dll" ]

    let missing = required |> List.filter (fun d -> not (Set.contains d dlls))
    test <@ List.isEmpty missing @>

[<Fact>]
let ``package omits host-provided assemblies to avoid load conflicts`` () =
    let dlls = analyzerPathDlls ()

    // These are supplied by the analyzer host (FSharp.Analyzers.SDK) or are build-time
    // only; bundling them would collide with the host's own copies.
    let hostProvided =
        [ "FSharp.Analyzers.SDK.dll"
          "FSharp.Compiler.Service.dll"
          "FSharp.Core.dll"
          "McMaster.NETCore.Plugins.dll"
          "Microsoft.Extensions.Logging.Abstractions.dll"
          "System.IO.Hashing.dll" ]

    let leaked = hostProvided |> List.filter (fun d -> Set.contains d dlls)
    test <@ List.isEmpty leaked @>
