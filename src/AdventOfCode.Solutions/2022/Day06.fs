namespace AdventOfCode.Solutions._2022

open AdventOfCode.Lib.Solver
open System

module Day06 =
    [<AocSolver(2022, 6, Level = 1)>]
    let solve1 (input: string) =
        input
        |> Seq.windowed 4
        |> Seq.findIndex (fun window -> Seq.distinct window |> Seq.length = 4)
        |> ((+) 4)
        
    [<AocSolver(2022, 6, Level = 2)>]
    let solve2 (input: string) =
        input
        |> Seq.windowed 14
        |> Seq.findIndex (fun window -> Seq.distinct window |> Seq.length = 14)
        |> ((+) 14)
