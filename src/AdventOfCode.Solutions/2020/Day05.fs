namespace AdventOfCode.Solutions._2020

open AdventOfCode.Lib.Solver
open System

module Day05 =
    let parseSeat (str: string) =
        let bStr = str.Replace('F', '0')
                      .Replace('B', '1')
                      .Replace('L', '0')
                      .Replace('R', '1')
        Convert.ToInt32(bStr, 2)
        
    [<AocSolver(2020, 5, Level = 1)>]
    let solve1 (input: string list) =
        input
        |> List.map parseSeat
        |> List.max
        
    [<AocSolver(2020, 5, Level = 2)>]
    let solve2 (input: string list) =
        let sorted = input
                     |> List.map parseSeat
                     |> List.sort
        let (a, _b) = Seq.zip sorted (Seq.skip 1 sorted)
                      |> Seq.find (fun (a, b) -> b - a = 2)
        a + 1