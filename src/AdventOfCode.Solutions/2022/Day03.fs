namespace AdventOfCode.Solutions._2022

open AdventOfCode.Lib.Solver
open System

module Day03 =
    let convertToPrio itemType =
        if Char.IsLower itemType then Convert.ToInt32(itemType) - Convert.ToInt32('a') + 1
        else Convert.ToInt32(itemType) - Convert.ToInt32('A') + 27
    
    let makeCompartments bag =
        bag
        |> Seq.map convertToPrio
        |> Seq.splitInto 2
        
    let processBag bag =
        bag
        |> makeCompartments
        |> Seq.map Set.ofSeq
        |> Set.intersectMany
        |> Set.sum
    
    [<AocSolver(2022, 3, Level = 1)>]
    let solve1 (input: string list) =
        input
        |> List.map processBag
        |> List.sum
        
    let processGroup group =
        group
        |> List.map Set.ofSeq
        |> Set.intersectMany
        |> Set.map convertToPrio
        |> Set.sum
        
    [<AocSolver(2022, 3, Level = 2)>]
    let solve2 (input: string list) =
        input
        |> List.chunkBySize 3
        |> List.map processGroup
        |> List.sum
        