namespace AdventOfCode.Solutions._2022

open AdventOfCode.Lib.Solver
open System

module Seq =
    let dump source =
        source |> Seq.map (fun e -> $"{e}")
        |> String.joinSeq "; "
        |> (fun s -> printfn $"{{{s}}}")
        source

module Day10 =
    let expandCommand cmd =
        match cmd |> String.split with
        | [|"noop"|] -> seq { yield (0, cmd) }
        | [|"addx"; n|] ->
            seq {
                yield (0, cmd)
                yield (Int32.parse n, cmd)
            }
        | _ -> failwith $"Invalid command: {cmd}"

    let xValues input =
        input |> Seq.map expandCommand
        |> Seq.concat
        |> Seq.toList
        |> Seq.scan (fun (start, finish, _) (n, cmd) -> (finish, finish + n, cmd)) (1, 1, "")
        |> Seq.skip 1 // We're not interested in the initial state
        
        
    [<AocSolver(2022, 10, Level = 1)>]
    let solve1 (input: string list) =
        let xValues = xValues input
        
        [20;60;100;140;180;220]
        |> Seq.map (fun n -> (n, xValues |> Seq.skip (n - 1) |> Seq.take 1 |> Seq.head))
        |> Seq.dump
        |> Seq.map (fun (i, (n, _, cmd)) -> i * n)
        |> Seq.dump
        |> Seq.sum
        
    [<AocSolver(2022, 10, Level = 2)>]
    let solve2 (input: string list) =
        // We have to divide the sequence by 240
        // Not really, but at least we need an index
        // We need to do something given an instruction and knowing how many instructions we have
        //  We once again want to SCAN because we need intermediate results
        // Given an instruction
        //   - its index % 240 gives us the index in the display
        //   - its X value determines the location of the sprite
        //   - its index % 40 gives us the index in the _line_ of the display
        //   So
        //     We make an array of size 240
        //     We update index % 240
        //       # if X - 1 <= lineIndex <= X + 1
        //       . otherwise
        // Then when we have the display
        //  We split it into 6
        //  And write each line
        let xValues =
            xValues input
            |> Seq.map (fun (x, _, _) -> x)
            |> Seq.indexed
        
        let update display (index, x) =
            let displayIndex = index % 240
            let spriteIndex = x
            let lineIndex = index % 40
            if lineIndex >= spriteIndex - 1 && lineIndex <= spriteIndex + 1 then
                Array.updateAt displayIndex '#' display
            else
                Array.updateAt displayIndex '.' display
        
        let display = Array.create 240 '.'
        
        xValues
        |> Seq.scan update display
        |> Seq.last
        |> Array.splitInto 6
        |> Array.map String.Concat
        |> String.joinSeq Environment.NewLine
            