namespace AdventOfCode.Solutions.Year2021

open AdventOfCode.Lib.Solver
open System

module Day01 =
    [<AocSolver(2021, 1, Level = 1)>]
    let solve1 (input: string list) =
        let values = List.map Int32.Parse input
        Seq.zip values (Seq.skip 1 values)
        |> Seq.where (fun (a, b) -> a < b)
        |> Seq.length
        
    [<AocSolver(2021, 1, Level = 2)>]
    let solve2 (input: string list) =
        let values = List.map Int32.Parse input
        Seq.zip values (Seq.skip 3 values)
        |> Seq.filter (fun (a, b) -> a < b)
        |> Seq.length
