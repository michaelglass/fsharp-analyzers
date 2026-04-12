module TestData.IgnoredTaskSuppressed

open System.Threading.Tasks

let doWork () =
    Task.Run(fun () -> printfn "working") |> ignore // MGA-TASK-IGNORE-001:ok
