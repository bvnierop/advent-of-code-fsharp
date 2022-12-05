namespace AdventOfCode.Solutions._2022

open AdventOfCode.Lib.Solver
open System

module Day05 =
    let parseContainerLine (line: string) =
        line.Replace("    ", " [-]")
            .Replace("[", " ")
            .Replace("]", " ")
            .Split(" ", StringSplitOptions.RemoveEmptyEntries)
            
    let parseContainerLines (lines: string list) =
        let asSegments = lines |> List.map parseContainerLine
        let len = Array.length asSegments[0]
        let stacks = Array.init len (fun _ -> [])
        let filledStacks =
            List.foldBack (fun (elt: string array) stacks ->
                let mutable x = stacks
                for i = 0 to len - 1 do
                    x <- (Array.updateAt i (elt[i] :: stacks[i]) x)
                x) asSegments stacks
        Array.map (List.reject ((=) "-")) filledStacks
        
    let parseIntOpt (str: string) =
        match Int32.TryParse(str) with
        | (true, value) -> Some value
        | (false, _) -> None
        
    type Move = {
        Count: int;
        Src: int;
        Dst: int;
    }
    
    let parseMoveLine (line: string) =
        match line.Split(" ") |> Array.choose parseIntOpt with
        | [|count;src;dst|] -> { Count = count; Src = src - 1; Dst = dst - 1 }
        | _ -> failwith $"Failed to parse move line: {line}"
        
    let parseMoveLines (lines: string list) =
        lines |> List.map parseMoveLine

    let parseInput (lines: string list) =
        match List.splitOnExclusive ((=) "") lines with
        | [containers;moves] -> (parseContainerLines containers, parseMoveLines moves)
        | _ -> failwith "Failed to parse"
        
    let step (move: Move) (stacks: string list array) =
        let mutable newStacks = stacks
        for i = 0 to move.Count - 1 do
            let container = List.head newStacks[move.Src]
            newStacks <- Array.updateAt move.Dst (container :: newStacks[move.Dst]) newStacks
            newStacks <- Array.updateAt move.Src (List.tail newStacks[move.Src]) newStacks
        newStacks
        
    let step2 (move: Move) (stacks: string list array) =
        let mutable newStacks = stacks
        let containers = List.take move.Count newStacks[move.Src]
        newStacks <- Array.updateAt move.Src (List.skip move.Count newStacks[move.Src]) newStacks
        newStacks <- Array.updateAt move.Dst (List.append containers newStacks[move.Dst]) newStacks
        newStacks
        
    let getTopOfStacks (stacks: string list array) =
        stacks
        |> Array.map List.head
        |> String.Concat
        
    [<AocSolver(2022, 5, Level = 1)>]
    let solve1 (input: string list) =
        let (stacks, moves) = parseInput input
        let mutable mutableStacks = stacks
        for move in moves do
            mutableStacks <- step move mutableStacks
        getTopOfStacks mutableStacks
        
    [<AocSolver(2022, 5, Level = 2)>]
    let solve2 (input: string list) =
        let (stacks, moves) = parseInput input
        let mutable mutableStacks = stacks
        for move in moves do
            mutableStacks <- step2 move mutableStacks
        getTopOfStacks mutableStacks
