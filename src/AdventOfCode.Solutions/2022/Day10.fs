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
        2