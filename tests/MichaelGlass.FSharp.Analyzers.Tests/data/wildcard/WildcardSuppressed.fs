module TestData.WildcardSuppressed

type Shape =
    | Circle
    | Square
    | Triangle

let describe (s: Shape) =
    match s with
    | Circle -> "round"
    | _ -> "not round" // MGA-WILDCARD-001:ok
