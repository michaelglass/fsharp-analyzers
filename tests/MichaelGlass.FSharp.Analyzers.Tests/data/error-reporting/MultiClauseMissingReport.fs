module TestData.MultiClauseMissingReport

let riskyOperation () =
    try
        failwith "boom"
    with
    | :? System.ArgumentException as ex -> printfn "arg: %A" ex
    | ex -> printfn "other: %A" ex
