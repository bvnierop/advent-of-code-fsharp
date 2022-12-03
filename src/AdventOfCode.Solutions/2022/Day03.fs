namespace AdventOfCode.Solutions._2022

open AdventOfCode.Lib.Solver
open System

module Day03 =
    let convertToPrio itemType =
        if Char.IsLower itemType then Convert.ToInt32(itemType) - Convert.ToInt32('a') + 1
        else Convert.ToInt32(itemType) - Convert.ToInt32('A') + 27
    
    [<AocSolver(2022, 3, Level = 1)>]
    let solve1 (input: string list) =
        input
        |> List.map List.ofSeq
        |> List.map (List.splitInto 2)
        |> List.map (List.map Set.ofList)
        |> List.map Set.intersectMany
        |> List.map Set.toList
        |> List.map (List.map convertToPrio)
        |> List.map List.sum
        |> List.sum
        
    [<AocSolver(2022, 3, Level = 2)>]
    let solve2 (input: string list) =
        input
        |> List.map List.ofSeq
        |> List.map Set.ofList
        |> List.chunkBySize 3
        |> List.map (fun group ->
            group
            |> Set.intersectMany
            |> Set.toList
            |> List.map convertToPrio
            |> List.sum)
        |> List.sum
        