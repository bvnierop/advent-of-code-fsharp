namespace AdventOfCode.Solutions._2021

open System
open AdventOfCode.Solutions.Solver

module Day06 =
    let parse (inp: string list) =
        let line = List.head inp
        line.Split(',')
        |> Array.map Int32.Parse
        |> Array.countBy id
        |> Array.fold (fun acc (idx, count) -> Array.updateAt idx (int64 count) acc) [|0;0;0;0;0;0;0;0;0|]
        
    let step arr =
        let head = Array.head arr
        let next = Array.append (Array.skip 1 arr) [|head|] 
        Array.updateAt 6 (next.[6] + head) next
        
    let solve input days =
        let fish = parse input
        [1..days]
        |> Seq.fold (fun acc _ -> step acc) fish
        |> Array.sum
        
    
    [<AocSolver(2021, 6, Level = 1)>]
    let solve1 (input: string list) =
        solve input 80
        
    [<AocSolver(2021, 6, Level = 2)>]
    let solve2 (input: string list) =
        solve input 256
