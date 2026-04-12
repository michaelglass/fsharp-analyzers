/// <summary>
/// Reads .editorconfig properties for analyzer configuration.
/// Uses the EditorConfig.Core library with per-file caching.
/// </summary>
module MichaelGlass.FSharp.Analyzers.EditorConfig

open System
open System.Collections.Concurrent
open EditorConfig.Core

let private parser = EditorConfigParser()

let private cache = ConcurrentDictionary<string, FileConfiguration>()

let private getParsed (fileName: string) =
    cache.GetOrAdd(fileName, fun f -> parser.Parse(f))

/// <summary>
/// Gets a single property value from .editorconfig for the given file.
/// </summary>
/// <param name="fileName">Absolute path to the source file being analyzed.</param>
/// <param name="key">The editorconfig property key (case-insensitive).</param>
/// <returns>The trimmed property value, or None if not present.</returns>
let getProperty (fileName: string) (key: string) : string option =
    try
        let configs = getParsed fileName

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
