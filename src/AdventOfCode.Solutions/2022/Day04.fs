namespace AdventOfCode.Solutions._2022

open AdventOfCode.Lib.Solver
open System

module Day04 =
    let parse (line: string) =
        match line.Split([|',';'-'|]) with
        | [|a;b;c;d|] -> ((Int32.Parse(a), Int32.Parse(b)),
                          (Int32.Parse(c), Int32.Parse(d)))
        | _ -> failwith $"Invalid input: {line}"
                         
    let isFullyOverlapping range1 range2 =
        match (range1, range2) with
        | ((l1, h1), (l2, h2)) when l1 <= l2 && h1 >= h2 -> true // range1 fully contains range 2
        | ((l1, h1), (l2, h2)) when l2 <= l1 && h2 >= h1 -> true // range2 fully contains range 1
        | _ -> false
        
    let isDisjoint range1 range2 =
        match (range1, range2) with
        | ((l1, h1), (l2, h2)) when h2 < l1 -> true
        | ((l1, h1), (l2, h2)) when h1 < l2 -> true
        | _ -> false
        
    [<AocSolver(2022, 4, Level = 1)>]
    let solve1 (input: string list) =
        input
        |> List.map parse
        |> List.countWhere (fun (r1, r2) -> isFullyOverlapping r1 r2)
    
    [<AocSolver(2022, 4, Level = 2)>]
    let solve2 (input: string list) =
        input
        |> List.map parse
        |> List.countWhere (fun (r1, r2) -> not (isDisjoint r1 r2))
