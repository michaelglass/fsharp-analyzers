module TestData.IgnoredRunSyncResult

// `Async.RunSynchronously` returns the unwrapped (synchronous) result, so
// `asyncExpr |> Async.RunSynchronously |> ignore` ignores a plain value, not a
// Task/Async. Must NOT be flagged. Repro: thellma/intelligence Program.fs.
let work () : Async<int> = async { return 1 }

let doIt () =
    work () |> Async.RunSynchronously |> ignore
