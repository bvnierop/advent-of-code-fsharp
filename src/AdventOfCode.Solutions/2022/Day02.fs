namespace AdventOfCode.Solutions._2022

open AdventOfCode.Lib.Solver
open System

module Day02 =
    [<AocSolver(2022, 2, Level = 1)>]
    let solve1 (input: string list) =
        let mutable score = 0
        for line in input do
            match line.Split(" ") with
            | [|"A"; "X"|] -> score <- score + 1 + 3
            | [|"A"; "Y"|] -> score <- score + 2 + 6
            | [|"A"; "Z"|] -> score <- score + 3 + 0
            | [|"B"; "X"|] -> score <- score + 1 + 0
            | [|"B"; "Y"|] -> score <- score + 2 + 3
            | [|"B"; "Z"|] -> score <- score + 3 + 6
            | [|"C"; "X"|] -> score <- score + 1 + 6
            | [|"C"; "Y"|] -> score <- score + 2 + 0
            | [|"C"; "Z"|] -> score <- score + 3 + 3
            | _ -> failwith "Impossible move"
        score
        
    [<AocSolver(2022, 2, Level = 2)>]
    let solve2 (input: string list) =
        let mutable score = 0
        for line in input do
            match line.Split(" ") with
            | [|"A"; "X"|] -> score <- score + 3 + 0
            | [|"A"; "Y"|] -> score <- score + 1 + 3
            | [|"A"; "Z"|] -> score <- score + 2 + 6
            | [|"B"; "X"|] -> score <- score + 1 + 0
            | [|"B"; "Y"|] -> score <- score + 2 + 3
            | [|"B"; "Z"|] -> score <- score + 3 + 6
            | [|"C"; "X"|] -> score <- score + 2 + 0
            | [|"C"; "Y"|] -> score <- score + 3 + 3
            | [|"C"; "Z"|] -> score <- score + 1 + 6
            | _ -> failwith "Impossible move"
        score
