module TestData.IgnoredTask

open System.Threading.Tasks

let doWork () =
    Task.Run(fun () -> printfn "working") |> ignore
