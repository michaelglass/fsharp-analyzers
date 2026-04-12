module TestData.HasErrorReport

let logError (ex: exn) = printfn "ERROR: %A" ex

let riskyOperation () =
    try
        failwith "boom"
    with ex ->
        logError ex
