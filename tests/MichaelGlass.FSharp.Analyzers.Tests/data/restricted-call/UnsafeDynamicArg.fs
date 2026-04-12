module TestData.UnsafeDynamicArg

module Text =
    let raw (s: string) = s

let bad (userInput: string) =
    Text.raw userInput |> ignore
