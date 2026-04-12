module TestData.WildcardOnDU

type Shape =
    | Circle
    | Square
    | Triangle

let describe (s: Shape) =
    match s with
    | Circle -> "round"
    | _ -> "not round"
