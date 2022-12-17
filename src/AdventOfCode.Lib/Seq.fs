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
    
let countWhere predicate = Seq.filter predicate >> Seq.length

let dump source =
    source |> Seq.map (fun e -> $"{e}")
    |> String.joinSeq "; "
    |> (fun s -> printfn $"{{{s}}}")
    source

let butLast source =
    Seq.take (Seq.length source - 1) source

/// Turns the `source` sequence into a cyclic sequence
let cyclic source =
    let rec next () = seq {
        for e in source do yield e
        yield! next ()
    }
    next ()
    
let cyclic2 source =
    let original = Seq.toList source
    match original with
    | head::tail ->
        Seq.unfold (fun remaining ->
            match remaining with
            | x::xs -> Some (x, xs)
            | [] -> Some (head, tail)) original
    | _ -> []
    