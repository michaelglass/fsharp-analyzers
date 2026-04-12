module MichaelGlass.FSharp.Analyzers.Tests.Common

open FSharp.Analyzers.SDK.Testing

let projectOptions =
    mkOptionsFromProject "net10.0" []
    |> Async.AwaitTask
    |> Async.RunSynchronously

let getContextForSource (source: string) =
    getContext projectOptions source
