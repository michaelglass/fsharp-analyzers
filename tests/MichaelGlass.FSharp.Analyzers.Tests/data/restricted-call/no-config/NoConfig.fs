module TestData.NoConfig

open System.Threading.Tasks

let fine () =
    Task.WhenAll([| Task.CompletedTask |]) |> ignore
