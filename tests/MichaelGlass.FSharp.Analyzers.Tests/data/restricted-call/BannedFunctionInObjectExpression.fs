module TestData.BannedFunctionInObjectExpression

open System.Threading.Tasks

let bad () =
    { new System.IDisposable with
        member _.Dispose() =
            Task.WhenAll([| Task.CompletedTask |]) |> ignore
    }
