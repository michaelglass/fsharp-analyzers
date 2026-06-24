module TestData.IgnoredAsync

// A real Async value ignored without running it: exceptions vanish. MUST flag.
let work () : Async<int> = async { return 1 }

let doWork () = work () |> ignore
