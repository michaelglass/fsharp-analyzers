module TestData.IgnoreShapes

open System.Threading.Tasks

let work () : Async<int> = async { return 1 }

// Each `// flag` line MUST be reported; each `// keep` line must NOT.
let directCall () = ignore (work ()) // flag
let qualifiedIgnore () = work () |> Operators.ignore // flag
let genericTask () = Task.FromResult 1 |> ignore // flag
let valueTask () = ValueTask.CompletedTask |> ignore // flag

let boundTask () =
    let t = Task.CompletedTask
    t |> ignore // flag

let parenthesised () = (work ()) |> ignore // flag
let annotated () = (work (): Async<int>) |> ignore // flag
let inAnonRecord () = {| Discarded = work () |> ignore |} // flag
let genericValue (x: 'a) = x |> ignore // keep
let directCallOnValue () = ignore (List.length [ 1 ]) // keep
