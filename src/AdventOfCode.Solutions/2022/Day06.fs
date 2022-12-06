namespace AdventOfCode.Solutions._2022

open AdventOfCode.Lib.Solver
open System

module Day06 =
    let findFirstUniqueSubsequence n seq =
        seq
        |> Seq.windowed n
        |> Seq.findIndex (fun window -> Seq.distinct window |> Seq.length = n)
        |> ((+) n)
        
    [<AocSolver(2022, 6, Level = 1)>]
    let solve1 (input: string) =
        findFirstUniqueSubsequence 4 input
        
    [<AocSolver(2022, 6, Level = 2)>]
    let solve2 (input: string) =
        findFirstUniqueSubsequence 14 input
