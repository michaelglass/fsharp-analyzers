# Changelog

## Unreleased

- fix: `.editorconfig` changes now take effect without restarting the analyzer host. Every `mga_*` setting was read exactly once per process and then answered from memory forever, so in a long-lived host (the fshw daemon) editing a setting re-ran the analyzers against the configuration as it stood hours earlier. The host's own result cache does cover the config files, so it correctly re-ran the analyzer after the edit — and then recorded the stale answer as the verdict for the NEW configuration, where it survived a daemon restart. A rule you had just switched on reported nothing found, which is indistinguishable from nothing being there. There were two caches, not one: a module-level dictionary keyed on the analysed file's path, and a shared `EditorConfigParser`, which resolves a directory's `.editorconfig` chain once and holds it for the life of the instance. The second is the one that bites hardest — it made even a file the process had NEVER read before answer from the old configuration, so removing only the dictionary would have looked like a fix and changed nothing. Both are gone: a lookup now builds its own parser and reads the chain as it is on disk. Measured on a 1894-file repository, the whole lookup costs ~0.07ms against ~0.006ms for the cached answer — about 0.8s added to a full analyzer pass, and nothing at all to the incremental case a daemon actually runs. A content fingerprint over the config chain was measured as the alternative and rejected: at ~0.12ms it cost more per lookup than simply re-reading. One narrow staleness window remains and belongs to EditorConfig.Core, which keys its own process-wide cache of parsed config files on path, size and last-write time: a config rewritten to different content of exactly the same length with its timestamp preserved is still served from that cache. Ordinary edits and checkouts move the timestamp; bypassing it measured 3.4x the cost of the lookup.

## 0.1.0-alpha.5 - 2026-08-19

- fix: a `// {CODE}:ok` marker now binds from anywhere in the contiguous comment block directly above the flagged construct, not only from the single adjacent line. Writing the marker first and the justification under it — the natural order — used to leave the marker two or more lines up, where nothing looked for it; the diagnostic then reappeared shifted down by the length of the justification, which reads like the analyzer moved to a new site rather than like the suppression missing. The block still ends at the first blank line or line of code, so a marker separated from its construct continues not to suppress, and every placement is pinned by a test in both directions.

## 0.1.0-alpha.4 - 2026-06-25

- fix: bundle EditorConfig.Core's transitive deps so `.editorconfig` MGA config (`mga_wildcard_allowed_types`, `mga_rawsql_excluded_files`, `mga_error_reporting_functions`, `mga_banned_*`) actually loads in an analyzer host instead of silently falling back to defaults. The package previously shipped only `EditorConfig.Core.dll`; inside an analyzer host (the fshw daemon) `EditorConfigParser()` then threw `FileNotFoundException` for the unbundled `TestableIO.System.IO.Abstractions.Wrappers`, which `getProperty` swallowed — so every MGA key fell back to its hardcoded default. The pack target now bundles the full runtime closure (everything the host does not already provide), and `getProperty` no longer masks a parser-construction failure.

## 0.1.0-alpha.3 - 2026-06-24

- fix: **MGA-RAWSQL-001** no longer flags English prose that merely starts with a SQL keyword (e.g. `"Create account"`, `"Delete Account"`) — keyword matching is now case-sensitive against upper-case (real raw SQL upper-cases keywords) and requires content beyond the bare keyword. This also recovers true positives the old heuristic missed (e.g. `SELECT 1`). **MGA-TASK-IGNORE-001** no longer mis-fires on a synchronous `… |> ignore` (e.g. `expr |> Async.RunSynchronously |> ignore`) — it now checks the return type of the result-producing function rather than any symbol within the ignored expression's range. Both were latent over-broad heuristics exposed by the 0.37.2 / FCS-43.12 recompile in alpha.2.

## 0.1.0-alpha.2 - 2026-06-24

- Bump MichaelsWackyFsPackageTools tools to latest alpha
- Bump FsSemanticTagger to 0.12.0-alpha.3
- Bump dotnet tools (coverageratchet 0.13.0-alpha.1, pin unchanged tools to latest published alpha.2)
- Update dotnet tools to 0.12.0-alpha.3 / 0.9.0-alpha.3
- Bump `FSharp.Analyzers.SDK` 0.36.0 → 0.37.2 (recompiled against FCS 43.12.201; adapted to the `SynExpr.LetOrUse` AST shape change — no diagnostic behavior change) + `editorconfig` 0.15.0 → 0.16.2

## v0.1.0-alpha.1

- Initial alpha release
