namespace AdventOfCode.Solutions._2022

open AdventOfCode.Lib.Solver
open System

module Day03 =
    let convertToPrio itemType =
        if Char.IsLower itemType then Convert.ToInt32(itemType) - Convert.ToInt32('a') + 1
        else Convert.ToInt32(itemType) - Convert.ToInt32('A') + 27
        
    let formGroups formGroupFn lines =
        lines
        |> List.map (Seq.map convertToPrio)
        |> formGroupFn
        |> List.map (Seq.map Set.ofSeq)
    
    let getSumOfIntersection groups =
        groups
        |> Seq.map (Set.intersectMany >> Seq.sum)
        |> Seq.sum
        
    [<AocSolver(2022, 3, Level = 1)>]
    let solve1 (input: string list) =
        input
        |> formGroups (List.map (Seq.splitInto 2))
        |> getSumOfIntersection
        
    [<AocSolver(2022, 3, Level = 2)>]
    let solve2 (input: string list) =
        input
        |> formGroups (List.chunkBySize 3)
        |> getSumOfIntersection
