/// <summary>
/// Reads .editorconfig properties for analyzer configuration.
/// Uses the EditorConfig.Core library.
/// </summary>
module MichaelGlass.FSharp.Analyzers.EditorConfig

open System
open EditorConfig.Core

/// <summary>
/// Builds the parser for a single lookup.
/// </summary>
/// <remarks>
/// <para>
/// Deliberately NOT a shared instance, and deliberately not memoised per file. An
/// analyzer host is a daemon: it loads this assembly once and keeps it for hours,
/// re-analyzing the same paths while the repository changes underneath it.
/// <c>EditorConfigParser</c> resolves the <c>.editorconfig</c> chain for a directory
/// on first access and then holds it for the life of the instance, so a shared parser
/// answers every later lookup from the configuration as it stood the first time it
/// looked. The host's own result cache does cover the config files, so it correctly
/// re-runs the analyzer after an edit — and then records THIS stale answer as the
/// verdict for the NEW configuration, where it outlives a daemon restart. A rule you
/// have just switched on reports nothing found, which reads exactly like nothing being
/// there. A parser costs about half a microsecond to construct and the whole lookup
/// about 0.07ms, which is the price of an answer that describes the config on disk now.
/// </para>
/// <para>
/// One staleness window is left, and it belongs to the library: EditorConfig.Core keeps
/// its own process-wide cache of parsed config FILES keyed on path, size and
/// last-write time. An <c>.editorconfig</c> rewritten to a different content of exactly
/// the same length with its timestamp preserved is therefore still served from that
/// cache. Ordinary edits and checkouts move the timestamp, so this is narrow; closing
/// it means passing a factory that re-reads unconditionally, which measured 3.4x the
/// cost of the whole lookup and buys nothing for how configuration actually changes.
/// </para>
/// <para>
/// Constructing <c>EditorConfigParser</c> loads EditorConfig.Core's transitive
/// dependencies (System.IO.Abstractions and friends). When this package is consumed
/// inside an analyzer host (the fshw daemon) those deps must be bundled alongside the
/// analyzer; if they are missing the constructor throws a <c>FileNotFoundException</c>
/// / <c>TypeInitializationException</c>. That is a packaging/deployment fault, NOT a
/// "no .editorconfig key" situation, so it is deliberately allowed to propagate — see
/// <see cref="getProperty"/>. Swallowing it (the original bug) made every MGA config
/// key silently fall back to its default.
/// </para>
/// </remarks>
let private newParser () = EditorConfigParser()

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
    // Construct first, OUTSIDE the property-lookup try/with: a construction failure
    // (missing deps / assembly load) must surface, not be swallowed as "no key".
    let parser = newParser ()

    try
        let configs = parser.Parse(fileName)

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
