module TestData.BannedCallPattern

module Attr =
    let type' (s: string) = s

let bad () = Attr.type' "submit" |> ignore
