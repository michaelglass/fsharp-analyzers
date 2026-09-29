module TestData.BannedFunctionInAnonRecord

open System.Threading.Tasks

let bad () =
    {|
        Pending = Task.WhenAll([| Task.CompletedTask |])
    |}
