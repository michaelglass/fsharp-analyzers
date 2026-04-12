module TestData.BannedFunction

open System.Threading.Tasks

let bad () =
    Task.WhenAll([| Task.CompletedTask |]) |> ignore
