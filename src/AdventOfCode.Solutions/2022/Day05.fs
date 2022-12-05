namespace AdventOfCode.Solutions._2022

open AdventOfCode.Lib.Solver
open FParsec
open System

module Day05 =
    type Move = {
        Count: int;
        Src: int;
        Dst: int;
    }
    
    let cratesToStacks crates =
        crates
        |> List.transpose
        |> List.map (List.choose id)
        |> List.toArray
        
    let parseEmptyCrate = pstring "   " >>% None
    let parseSingleCrate = (skipAnyChar >>. anyString 1 .>> skipAnyChar) |>> Some
    let parseCrate = parseEmptyCrate <|> parseSingleCrate
    let parseCrates = sepBy parseCrate (pchar ' ')
    let parseCrateLine = parseCrates .>> skipNewline
    let parseStacks = (manyTill parseCrateLine newline) |>> cratesToStacks
    let parseMove =
        pipe3 (skipString "move " >>. pint32)
              (skipString " from " >>. pint32)
              (skipString " to " >>. pint32)
              (fun a b c -> {Count = a; Src = b - 1; Dst = c - 1})
    let parseMoveLine = parseMove .>> (skipNewline <|> eof)
    let parseInput =  parseStacks .>>. many parseMoveLine
        
    let step reorder (stacks: string list array) move =
        let containers = List.take move.Count stacks[move.Src] |> reorder
        stacks
        |> Array.updateAt move.Src (List.skip move.Count stacks[move.Src])
        |> Array.updateAt move.Dst (List.append containers stacks[move.Dst])
        
    let getTopOfStacks (stacks: string list array) =
        stacks
        |> Array.map List.head
        |> String.Concat
        
    [<AocSolver(2022, 5, Level = 1)>]
    let solve1 (input: string) =
        let (stacks, moves) = parseOrDie parseInput input
        moves
        |> List.fold (step List.rev) stacks
        |> getTopOfStacks
        
    [<AocSolver(2022, 5, Level = 2)>]
    let solve2 (input: string) =
        let (stacks, moves) = parseOrDie parseInput input
        moves
        |> List.fold (step id) stacks
        |> getTopOfStacks
