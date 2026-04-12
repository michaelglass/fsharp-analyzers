module TestData.IgnoredNonTask

let doWork () =
    [ 1; 2; 3 ] |> List.map string |> ignore
