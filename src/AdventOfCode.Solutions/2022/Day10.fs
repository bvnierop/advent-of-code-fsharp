namespace AdventOfCode.Solutions._2022

open AdventOfCode.Lib.Solver
open System

module Day10 =
    let expandCommand cmd =
        match cmd |> String.split with
        | [|"noop"|] -> seq { yield 0 }
        | [|"addx"; n|] ->
            seq {
                yield 0
                yield Int32.parse n
            }
        | _ -> failwith $"Invalid command: {cmd}"

    let xValues input =
        input |> Seq.map expandCommand
        |> Seq.concat
        |> Seq.toList
        |> Seq.scan (+) 1
        
    [<AocSolver(2022, 10, Level = 1)>]
    let solve1 (input: string list) =
        let xValues = xValues input
        
        [20;60;100;140;180;220]
        |> Seq.map (fun n -> (n, xValues |> Seq.skip (n - 1) |> Seq.take 1 |> Seq.head))
        |> Seq.map (fun (i, n) -> i * n)
        |> Seq.sum
        
    [<AocSolver(2022, 10, Level = 2)>]
    let solve2 (input: string list) =
        let xValues =
            xValues input
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
            