/// <summary>
/// Reads .editorconfig properties for analyzer configuration.
/// Uses the EditorConfig.Core library with per-file caching.
/// </summary>
module MichaelGlass.FSharp.Analyzers.EditorConfig

open System
open System.Collections.Concurrent
open EditorConfig.Core

/// <summary>
/// The shared parser, constructed once on first use.
/// </summary>
/// <remarks>
/// Constructing <c>EditorConfigParser</c> loads EditorConfig.Core's transitive
/// dependencies (System.IO.Abstractions and friends). When this package is consumed
/// inside an analyzer host (the fshw daemon) those deps must be bundled alongside the
/// analyzer; if they are missing the constructor throws a <c>FileNotFoundException</c>
/// / <c>TypeInitializationException</c>. That is a packaging/deployment fault, NOT a
/// "no .editorconfig key" situation, so it is deliberately allowed to propagate — see
/// <see cref="getProperty"/>. Swallowing it (the original bug) made every MGA config
/// key silently fall back to its default.
/// </remarks>
let private parser = lazy EditorConfigParser()

let private cache = ConcurrentDictionary<string, FileConfiguration>()

/// <summary>
/// Gets a single property value from .editorconfig for the given file.
/// </summary>
/// <param name="fileName">Absolute path to the source file being analyzed.</param>
/// <param name="key">The editorconfig property key (case-insensitive).</param>
/// <returns>The trimmed property value, or None if the key is genuinely absent.</returns>
/// <remarks>
/// A genuinely missing key (or a per-file parse failure of a malformed .editorconfig)
/// degrades to <c>None</c>. A parser-construction failure — a missing transitive
/// dependency or other assembly-load fault — is rethrown rather than masked, so a
/// broken deployment fails loudly instead of silently using defaults.
/// </remarks>
let getProperty (fileName: string) (key: string) : string option =
    // Force construction first, OUTSIDE the property-lookup try/with: a construction
    // failure (missing deps / assembly load) must surface, not be swallowed as "no key".
    let parser = parser.Value

    try
        let configs = cache.GetOrAdd(fileName, fun f -> parser.Parse(f))

        configs.Properties
        |> Seq.tryFind (fun kvp -> kvp.Key.Equals(key, StringComparison.OrdinalIgnoreCase))
        |> Option.map (fun kvp -> kvp.Value.Trim())
    with _ ->
        None

/// <summary>
/// Gets a comma-separated list property from .editorconfig.
/// </summary>
/// <param name="fileName">Absolute path to the source file being analyzed.</param>
/// <param name="key">The editorconfig property key (case-insensitive).</param>
/// <returns>List of trimmed values, or empty list if the key is not present.</returns>
let getListProperty (fileName: string) (key: string) : string list =
    match getProperty fileName key with
    | None -> []
    | Some value ->
        value.Split(',', StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)
        |> Array.toList
