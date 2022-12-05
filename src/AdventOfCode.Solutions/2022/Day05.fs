namespace AdventOfCode.Solutions._2022

open AdventOfCode.Lib.Solver
open System

module Day05 =
    let parseContainerLine (line: string) =
        line.Replace("    ", " [-]")
            .Replace("[", " ")
            .Replace("]", " ")
            .Split(" ", StringSplitOptions.RemoveEmptyEntries) |> Array.toList
            
    let parseContainerLines (lines: string list) =
        lines
        |> List.map parseContainerLine
        |> List.transpose
        |> List.toArray
        |> Array.map (List.reject ((=) "-"))
        
    type Move = {
        Count: int;
        Src: int;
        Dst: int;
    }
    
    let parseMoveLine (line: string) =
        match line.Split(" ") |> Array.choose Int32.parseOpt with
        | [|count;src;dst|] -> { Count = count; Src = src - 1; Dst = dst - 1 }
        | _ -> failwith $"Failed to parse move line: {line}"
        
    let parseMoveLines (lines: string list) =
        lines |> List.map parseMoveLine

    let parseInput (lines: string list) =
        match List.splitOnExclusive ((=) "") lines with
        | [containers;moves] -> (parseContainerLines containers, parseMoveLines moves)
        | _ -> failwith "Failed to parse"
        
    let step (stacks: string list array) move =
        let containers = List.take move.Count stacks[move.Src] |> List.rev
        stacks
        |> Array.updateAt move.Src (List.skip move.Count stacks[move.Src])
        |> Array.updateAt move.Dst (List.append containers stacks[move.Dst])
        
    let step2 (stacks: string list array) move =
        let containers = List.take move.Count stacks[move.Src]
        stacks
        |> Array.updateAt move.Src (List.skip move.Count stacks[move.Src])
        |> Array.updateAt move.Dst (List.append containers stacks[move.Dst])
        
    let getTopOfStacks (stacks: string list array) =
        stacks
        |> Array.map List.head
        |> String.Concat
        
    [<AocSolver(2022, 5, Level = 1)>]
    let solve1 (input: string list) =
        let (stacks, moves) = parseInput input
        moves
        |> List.fold step stacks
        |> getTopOfStacks
        
    [<AocSolver(2022, 5, Level = 2)>]
    let solve2 (input: string list) =
        let (stacks, moves) = parseInput input
        moves
        |> List.fold step2 stacks
        |> getTopOfStacks
