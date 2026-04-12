module TestData.NoConfig

let riskyOperation () =
    try
        failwith "boom"
    with ex ->
        printfn "caught: %A" ex
