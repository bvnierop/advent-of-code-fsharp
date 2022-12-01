module Seq

let splitOn predicate source =
    let mutable i = 0
    source
    |> Seq.groupBy (fun e ->
        if predicate e then i <- i + 1
        i)
    |> Seq.map snd
        
let reject predicate source = Seq.filter (predicate >> not) source

let splitOnExclusive predicate source =
    let mutable i = 0
    source
    |> Seq.groupBy (fun e ->
        if predicate e then
            i <- i + 1
            -1
        else
            i)
    |> reject (fun (idx, _) -> idx = -1)
    |> Seq.map snd