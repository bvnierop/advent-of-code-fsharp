namespace AdventOfCode.Solutions._2018

open AdventOfCode.Lib .Solver

module Day02 =
    
    let containsAnyNTimes n seq =
        Seq.countBy id seq
        |> Seq.exists (fun (_chr, count) -> count = n)
        
    [<AocSolver(2018, 2, Level = 1)>]
    let solve1 (input: string list) =
        let twos = List.filter (containsAnyNTimes 2) input |> List.length
        let threes = List.filter (containsAnyNTimes 3) input |> List.length
        twos * threes

    let strDist s1 s2 =
        Seq.zip s1 s2
        |> Seq.filter (fun (a, b) -> a <> b)
        |> Seq.length
        
    let strCommon (s1, s2) =
        Seq.zip s1 s2
        |> Seq.choose (fun (a, b) -> if a = b then Some a else None)
        |> Seq.toArray
        |> System.String
        
    [<AocSolver(2018, 2, Level = 2)>]
    let solve2 (input: string list) =
        let pairs = Seq.allPairs input input
        let ones = Seq.filter (fun (s1, s2) -> strDist s1 s2 = 1) pairs
        strCommon (Seq.head ones)
