module MichaelGlass.FSharp.Analyzers.EditorConfig

open System
open EditorConfig.Core

let private parser = EditorConfigParser()

/// Get a single property value from .editorconfig for the given file path.
let getProperty (fileName: string) (key: string) : string option =
    try
        let configs = parser.Parse(fileName)
        configs.Properties
        |> Seq.tryFind (fun kvp -> kvp.Key.Equals(key, StringComparison.OrdinalIgnoreCase))
        |> Option.map (fun kvp -> kvp.Value.Trim())
    with _ ->
        None

/// Get a comma-separated list property from .editorconfig.
/// Returns an empty list if the key is not present.
let getListProperty (fileName: string) (key: string) : string list =
    match getProperty fileName key with
    | None -> []
    | Some value ->
        value.Split(',', StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)
        |> Array.toList
