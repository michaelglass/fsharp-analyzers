module TestData.BannedFunctionPiped

open System.Threading.Tasks

let bad () =
    [| Task.CompletedTask |] |> Task.WhenAll |> ignore
