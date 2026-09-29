module TestData.HandlerShapes

// Every reporting handler below reports only from inside some nested shape. A `try`
// whose body is marked `// reports` must NOT be flagged; one marked `// silent` MUST be.

module Log =
    let logError (ex: exn) = printfn "ERROR: %A" ex

type Logger() =
    member _.captureError(ex: exn) = printfn "ERROR: %A" ex

let logError (ex: exn) = Log.logError ex
let logger = Logger()
let risky () : int = failwith "boom"

let qualified () =
    try
        risky () |> ignore // reports
    with ex ->
        Log.logError ex

let viaMethod () =
    try
        risky () |> ignore // reports
    with ex ->
        logger.captureError ex

let afterOtherWork () =
    try
        risky () |> ignore // reports
    with ex ->
        printfn "cleaning up"
        logError ex

let insideLet () =
    try
        risky () |> ignore // reports
    with ex ->
        let report = fun () -> logError ex
        report ()

let inOneBranch (retry: bool) =
    try
        risky () |> ignore // reports
    with ex ->
        if retry then () else logError ex

let inElseLessIf (loud: bool) =
    try
        risky () |> ignore // reports
    with ex ->
        if loud then
            logError ex

let inMatchArm () =
    try
        risky () |> ignore // reports
    with ex ->
        match ex with
        | :? System.TimeoutException -> ()
        | other -> logError other

let inNestedTryFinally () =
    try
        risky () |> ignore // reports
    with ex ->
        try
            logError ex
        finally
            printfn "done"

let inParens () =
    try
        risky () // reports
    with ex ->
        (logError ex
         0)

let inTuple () =
    try
        (risky (), ()) // reports
    with ex ->
        (0, logError ex)

let inTypedExpr () =
    try
        risky () |> ignore // reports
    with ex ->
        (logError ex: unit)

let inAsync () =
    async {
        try
            return risky () // reports
        with ex ->
            do! async { logError ex }
            return 0
    }

let inLoop (errors: exn list) =
    try
        risky () |> ignore // reports
    with _ ->
        for e in errors do
            logError e

let inList () =
    try
        [ risky () ] // reports
    with ex ->
        [
            (logError ex
             0)
        ]

let inRecordField () =
    try
        {| Value = risky () |} // reports
    with ex ->
        {|
            Value =
                (logError ex
                 0)
        |}

type Outcome = { Value: int }

let inNominalRecord () =
    try
        { Value = risky () } // reports
    with ex ->
        {
            Value =
                (logError ex
                 0)
        }

let inArray () =
    try
        [| risky () |] // reports
    with ex ->
        [|
            (logError ex
             0)
        |]

let getLogger () = logger

let viaReceiverExpression () =
    try
        risky () |> ignore // reports
    with ex ->
        (getLogger ()).captureError ex

let inLetBody () =
    try
        risky () |> ignore // reports
    with ex ->
        let message = ex.Message
        printfn "%s" message
        logError ex

let inNestedTryWith () =
    try
        risky () |> ignore // reports
    with ex ->
        try
            logError ex
        with _ ->
            logError ex

let inReturn () =
    async {
        try
            return risky () // reports
        with ex ->
            return
                (logError ex
                 0)
    }

let silent () =
    try
        risky () |> ignore // silent
    with ex ->
        printfn "%s" ex.Message

let reportsInTryBodyOnly () =
    try
        logError (exn "not the handler") // silent
    with _ ->
        ()
