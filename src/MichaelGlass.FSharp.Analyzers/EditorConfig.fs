/// <summary>
/// Reads .editorconfig properties for analyzer configuration.
/// Uses the EditorConfig.Core library.
/// </summary>
module MichaelGlass.FSharp.Analyzers.EditorConfig

open System
open System.Collections.Concurrent
open System.IO.Abstractions
open EditorConfig.Core

let private fileSystem = FileSystem()

/// <summary>
/// Parsed <c>.editorconfig</c> files, shared by every lookup in the process: one entry
/// per config path, holding the parse together with the (last-write ticks, length) it
/// was read at.
/// </summary>
/// <remarks>
/// <see cref="newParser"/> builds a parser per lookup, so without a shared cache every
/// lookup would re-read and re-parse each config file in its chain. EditorConfig.Core's
/// own <c>EditorConfigFileCache</c> keys entries on path, last-write time AND length and
/// never evicts, so in a long-lived host every edit of a config file would add an entry
/// that is never released. Keying on the path alone and replacing the entry when the
/// stamp moves keeps this bounded by the number of distinct config files.
/// </remarks>
let private parsedFiles =
    ConcurrentDictionary<string, struct (struct (int64 * int64) * EditorConfigFile)>(StringComparer.Ordinal)

/// <summary>The config paths currently held in the parsed-file cache.</summary>
let internal cachedConfigPaths () : string seq = parsedFiles.Keys

/// <summary>
/// Returns the parse of the config file at <paramref name="path"/>, re-reading it only
/// when its last-write time or length changed since the cached parse.
/// </summary>
/// <remarks>
/// The stamp is read BEFORE the content, so a write racing the read leaves an entry
/// whose stamp is older than its content, and the next lookup re-reads it.
/// </remarks>
let private parsedConfigFile (path: string) : EditorConfigFile =
    let info = fileSystem.FileInfo.New path
    let stamp = struct (info.LastWriteTimeUtc.Ticks, info.Length)

    match parsedFiles.TryGetValue path with
    | true, struct (cachedStamp, parsed) when cachedStamp = stamp -> parsed
    | _ ->
        let parsed = EditorConfigFile.Parse(path, fileSystem)
        parsedFiles[path] <- struct (stamp, parsed)
        parsed

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
/// The parsed config FILES, unlike the resolved chain, are shared: every parser reads
/// them through <see cref="parsedConfigFile"/>, which re-parses a file only when its
/// size or last-write time moved. That leaves one staleness window: an
/// <c>.editorconfig</c> rewritten to a different content of exactly the same length
/// with its timestamp preserved is still served from the cache. Ordinary edits and
/// checkouts move the timestamp, so this is narrow; closing it means re-reading every
/// config file on every lookup, which measured 3.4x the cost of the whole lookup and
/// buys nothing for how configuration actually changes.
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
let private newParser () =
    EditorConfigParser(Func<string, EditorConfigFile> parsedConfigFile, fileSystem = fileSystem)

/// <summary>
/// The .editorconfig properties that apply to the given file, resolved once. Look keys up
/// with <see cref="tryFind"/> and <see cref="listValue"/>.
/// </summary>
/// <remarks>
/// Resolving walks and matches the whole config chain, so a caller needing several keys
/// should resolve once and read each key from the result.
/// </remarks>
type Properties = private Properties of Map<string, string>

/// <summary>
/// Resolves the .editorconfig properties that apply to the given file.
/// </summary>
/// <param name="fileName">Absolute path to the source file being analyzed.</param>
/// <returns>The file's properties; empty when none apply.</returns>
/// <remarks>
/// A per-file failure (an unusable path, a malformed .editorconfig) degrades to no
/// properties. A parser-construction failure — a missing transitive dependency or other
/// assembly-load fault — is rethrown rather than masked, so a broken deployment fails
/// loudly instead of silently using defaults.
/// </remarks>
let getProperties (fileName: string) : Properties =
    // Construct first, OUTSIDE the property-lookup try/with: a construction failure
    // (missing deps / assembly load) must surface, not be swallowed as "no key".
    let parser = newParser ()

    try
        parser.Parse(fileName).Properties
        |> Seq.map (fun kvp -> kvp.Key.ToLowerInvariant(), kvp.Value.Trim())
        |> Map.ofSeq
        |> Properties
    with _ ->
        Properties Map.empty

/// <summary>A single property value.</summary>
/// <param name="key">The editorconfig property key (case-insensitive).</param>
/// <param name="properties">Properties from <see cref="getProperties"/>.</param>
/// <returns>The trimmed property value, or None if the key is absent.</returns>
let tryFind (key: string) (properties: Properties) : string option =
    let (Properties byKey) = properties
    Map.tryFind (key.ToLowerInvariant()) byKey

/// <summary>A comma-separated list property.</summary>
/// <param name="key">The editorconfig property key (case-insensitive).</param>
/// <param name="properties">Properties from <see cref="getProperties"/>.</param>
/// <returns>List of trimmed values, or empty list if the key is absent.</returns>
let listValue (key: string) (properties: Properties) : string list =
    match tryFind key properties with
    | None -> []
    | Some value ->
        value.Split(',', StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)
        |> Array.toList

/// <summary>
/// Gets a single property value from .editorconfig for the given file.
/// </summary>
/// <param name="fileName">Absolute path to the source file being analyzed.</param>
/// <param name="key">The editorconfig property key (case-insensitive).</param>
/// <returns>The trimmed property value, or None if the key is genuinely absent.</returns>
/// <remarks>Failure semantics as for <see cref="getProperties"/>.</remarks>
let getProperty (fileName: string) (key: string) : string option = getProperties fileName |> tryFind key

/// <summary>
/// Gets a comma-separated list property from .editorconfig.
/// </summary>
/// <param name="fileName">Absolute path to the source file being analyzed.</param>
/// <param name="key">The editorconfig property key (case-insensitive).</param>
/// <returns>List of trimmed values, or empty list if the key is not present.</returns>
let getListProperty (fileName: string) (key: string) : string list = getProperties fileName |> listValue key
