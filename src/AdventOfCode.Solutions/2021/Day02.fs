namespace AdventOfCode.Solutions._2021

open AdventOfCode.Lib.Solver
open System

module Day02 =
        
    let parseCommand (cmd: String) =
        match  cmd.Split(" ") |> Array.toList with
        | [x; y] -> (x, Int32.Parse y)
        | _ -> failwith "Failed to parse command"
            
    [<AocSolver(2021, 2, Level = 1)>]
    let solve1 (input: string list) =
        let mutable depth = 0
        let mutable pos = 0
        
        for command in input do
            match parseCommand command with
            | ("forward", n) -> pos <- pos + n
            | ("down", n) -> depth <- depth + n
            | ("up", n) -> depth <- depth - n
            | (cmd, _) -> failwith $"Unknown command: {cmd}"
            
        pos * depth
        
    [<AocSolver(2021, 2, Level = 2)>]
    let solve2 (input: string list) =
        let mutable depth = 0
        let mutable pos = 0
        let mutable aim = 0
        
        for command in input do
            match parseCommand command with
            | ("forward", n) -> pos <- pos + n; depth <- depth + (n * aim)
            | ("down", n) -> aim <- aim + n
            | ("up", n) -> aim <- aim - n
            | (cmd, _) -> failwith $"Unknown command: {cmd}"
        
        pos * depth
