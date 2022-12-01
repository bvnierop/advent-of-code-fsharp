namespace AdventOfCode.Solutions._2022

open AdventOfCode.Lib.Solver
open System

module Day01 =
    let caloriesPerElf input = 
        input
        |> Seq.splitOnExclusive ((=) "")
        |> Seq.map (Seq.map Int32.Parse)
        |> Seq.map (Seq.sum)
        
    [<AocSolver(2022, 1, Level = 1)>]
    let solve1 (input: string list) =
        input
        |> caloriesPerElf
        |> Seq.max
                    
    [<AocSolver(2022, 1, Level = 2)>]
    let solve2 (input: string list) =
        input
        |> caloriesPerElf
        |> Seq.sortDescending
        |> Seq.take 3
        |> Seq.sum
        