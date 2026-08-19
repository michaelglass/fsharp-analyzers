# MichaelGlass.FSharp.Analyzers

F# analyzers that catch bugs the compiler can't: silent exception swallowing, hidden match cases, raw SQL, and missing error reporting.

Built on [FSharp.Analyzers.SDK](https://github.com/ionide/FSharp.Analyzers.SDK). Configured via `.editorconfig`. Every diagnostic can be suppressed inline with `// {CODE}:ok`.

## Install

```
dotnet add package MichaelGlass.FSharp.Analyzers
```

Then [enable the SDK](https://ionide.io/FSharp.Analyzers.SDK/content/Getting%20Started%20Analyzers.html) in your editor or CI.

## Analyzers

| Code | Name | What it catches | Why it matters | Config |
|------|------|----------------|----------------|--------|
| `MGA-WILDCARD-001` | **Wildcard on DU** | `| _ ->` on discriminated unions | New union cases silently fall into the catch-all instead of producing a compiler warning | `mga_wildcard_allowed_types` |
| `MGA-TASK-IGNORE-001` | **Task Ignore** | `Task.Run(...) \|> ignore` | Exceptions in the ignored task vanish — no crash, no log, no trace | _(always on)_ |
| `MGA-RAWSQL-001` | **Raw SQL** | String literals starting with SQL keywords | Raw SQL is injection-prone, invisible to the type system, and breaks silently on schema changes | `mga_rawsql_excluded_files` |
| `MGA-ERROR-REPORT-001` | **Error Reporting** | `try/with` blocks missing an error-reporting call | Silent catches hide production failures from your observability stack | `mga_error_reporting_functions` |
| `MGA-UNSAFE-CALL-001` | **Restricted Call** | Banned functions, banned call patterns, unsafe dynamic args | Project-specific guardrails for dangerous APIs (XSS, concurrency, etc.) | `mga_banned_functions`, `mga_banned_call_patterns`, `mga_unsafe_dynamic_arg_functions` |

### Always-on analyzers

**Wildcard on DU** and **Task Ignore** and **Raw SQL** are enabled by default. Option, Result, and Choice types are allowed by the wildcard analyzer — add more via `.editorconfig`:

```ini
[*.fs]
mga_wildcard_allowed_types = MyApp.ParseResult, MyApp.Token
mga_rawsql_excluded_files = Migrations.fs
```

### Opt-in analyzers

**Error Reporting** and **Restricted Call** do nothing until configured:

```ini
[*.fs]
# Error Reporting: require these functions in every catch handler
mga_error_reporting_functions = captureError, logError

# Restricted Call: ban specific functions
mga_banned_functions = Task.WhenAll, Thread.Sleep

# Restricted Call: ban function + argument combinations
mga_banned_call_patterns = Attr.type':submit

# Restricted Call: flag non-literal args to injection-sensitive functions
mga_unsafe_dynamic_arg_functions = Text.raw, Html.rawText
```

## Suppression

Add `// {CODE}:ok` on the flagged line, or anywhere in the comment block directly above it:

```fsharp
match shape with
| Circle r -> drawCircle r
| _ -> drawDefault () // MGA-WILDCARD-001:ok — fallback is intentional here
```

A justification too long for one line goes on its own lines. The marker binds from anywhere
in that block, so it can lead the justification or trail it:

```fsharp
// MGA-ERROR-REPORT-001:ok — a part we cannot decode contributes no fields;
// the caller's report is still recorded from whatever else parsed.
try
    decodePart raw
with _ ->
    []
```

The block ends at the first blank line or line of code. A marker separated from the
construct that way does not suppress — which is what stops a marker left behind by an
earlier edit from silencing whatever moved in underneath it.

## License

MIT
