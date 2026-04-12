module MichaelGlass.FSharp.Analyzers.Tests.Common

open FSharp.Analyzers.SDK.Testing

let projectOptions =
    mkOptionsFromProject "net10.0" [] |> Async.AwaitTask |> Async.RunSynchronously

let getContextForSource (source: string) = getContext projectOptions source

let readTestData (parts: string list) =
    let path =
        System.IO.Path.Combine [| yield __SOURCE_DIRECTORY__; yield "data"; yield! parts |]

    System.IO.File.ReadAllText path
