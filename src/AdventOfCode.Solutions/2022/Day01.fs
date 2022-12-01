namespace AdventOfCode.Solutions._2022

open AdventOfCode.Lib.Solver
open System

module Day01 =
    let splitBy fn seq =
        let i = ref 0
        seq
        |> Seq.groupBy (fun e ->
            if fn e then incr i
            !i)
        |> Seq.map snd
        
    let caloriesPerElf input = 
        input
        |> splitBy ((=) "")
        |> Seq.map (Seq.filter ((<>) ""))
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