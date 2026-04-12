module TestData.SafeStaticArg

module Text =
    let raw (s: string) = s

let fine () =
    Text.raw "<br />" |> ignore
