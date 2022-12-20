module AdventOfCode.Solutions._2022.Day20

open System.Runtime.InteropServices
open AdventOfCode.Lib.Solver
open System

let remE a b = ((a % b) + b) % b

let solve input mixes (modifier: int64 -> int64) =
    let numbers = input |> List.map Int64.parse |> List.map modifier |> List.toArray
    let n = Array.length numbers
    let mutable positions = [| for i = 0 to n - 1 do int64 i |]
    
    let rebuild () =
        let mutable final = Array.copy numbers
        [0..n-1]
        |> List.iter (fun i ->
            let pos = positions[i]
            let number = numbers[i]
            final[int pos] <- number)
        final
    
    for _ = 1 to mixes do
        for i = 0 to n - 1 do
            let number = numbers[i]
            
            let currentPosition = positions[i]
            let mutable newPosition = remE (currentPosition + number) (int64 (n - 1))
            
            if newPosition = 0 && currentPosition <> newPosition then newPosition <- (int64 (n - 1))
            
            
            for j = 0 to n - 1 do
                if positions[j] > currentPosition then positions[j] <- positions[j] - 1L
                
            for j = 0 to n - 1 do
                if positions[j] >= newPosition then positions[j] <- remE (positions[j] + 1L) n
            positions[i] <- newPosition
        
    let res = rebuild ()
    let zero = Array.findIndex ((=) 0L) res
    
    res[(zero + 1000) % n] + res[(zero + 2000) % n] + res[(zero + 3000) % n]


[<AocSolver(2022, 20, Level = 1)>]
let solve1 (input: string list) =
    solve input 1 (fun i -> i)
    
    
[<AocSolver(2022, 20, Level = 2)>]
let solve2 (input: string list) =
    solve input 10 ((*) 811589153L)