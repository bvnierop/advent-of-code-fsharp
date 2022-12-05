module List

let splitOn predicate source =
    let mutable i = 0
    source
    |> List.groupBy (fun e ->
        if predicate e then i <- i + 1
        i)
    |> List.map snd
        
let reject predicate source = List.filter (predicate >> not) source

let splitOnExclusive predicate source =
    let mutable i = 0
    source
    |> List.groupBy (fun e ->
        if predicate e then
            i <- i + 1
            -1
        else
            i)
    |> reject (fun (idx, _) -> idx = -1)
    |> List.map snd
    
let countWhere predicate = List.filter predicate >> List.length
