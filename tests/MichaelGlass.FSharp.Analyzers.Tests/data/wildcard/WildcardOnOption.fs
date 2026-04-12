module TestData.WildcardOnOption

let describe (x: int option) =
    match x with
    | Some v -> sprintf "has %d" v
    | _ -> "none"
