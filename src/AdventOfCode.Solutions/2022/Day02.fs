namespace AdventOfCode.Solutions._2022

open AdventOfCode.Lib.Solver
open System

module Day02 =
    [<AocSolver(2022, 2, Level = 1)>]
    let solve1 (input: string list) =
        List.fold (fun score (line: string) ->
            match line with
            | "A X" -> score + 1 + 3
            | "A Y" -> score + 2 + 6
            | "A Z" -> score + 3 + 0
            | "B X" -> score + 1 + 0
            | "B Y" -> score + 2 + 3
            | "B Z" -> score + 3 + 6
            | "C X" -> score + 1 + 6
            | "C Y" -> score + 2 + 0
            | "C Z" -> score + 3 + 3
            | _ -> failwith "Impossible move") 0 input
        
    [<AocSolver(2022, 2, Level = 2)>]
    let solve2 (input: string list) =
        List.fold (fun score (line: string) ->
            match line with
            | "A X" -> score + 3 + 0
            | "A Y" -> score + 1 + 3
            | "A Z" -> score + 2 + 6
            | "B X" -> score + 1 + 0
            | "B Y" -> score + 2 + 3
            | "B Z" -> score + 3 + 6
            | "C X" -> score + 2 + 0
            | "C Y" -> score + 3 + 3
            | "C Z" -> score + 1 + 6
            | _ -> failwith "Impossible move") 0 input
