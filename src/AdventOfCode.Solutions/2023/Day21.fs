module AdventOfCode.Solutions._2023.Day21

open AdventOfCode.Lib.Solver
open System

[<AocSolver(2023, 21, Level = 1)>]
let solve1 (input: string list) =
    let map = input |> array2D
    let mutable S = (0,0)

    for y = 0 to Array2D.length1 map - 1 do
        for  x = 0 to Array2D.length2 map - 1 do
            if map[y,x] = 'S' then S <- (x,y)

    let Q = System.Collections.Generic.Queue()
    let seen = System.Collections.Generic.HashSet()
    let q e =
        if not (seen.Contains(e)) then
            Q.Enqueue(e)
            seen.Add(e) |> ignore
    let res = System.Collections.Generic.HashSet()

    q (S, 0)
    while Q.Count > 0 do
        let (x, y), d = Q.Dequeue()
        let neighbors = [(x+1,y); (x-1,y); (x,y+1); (x,y-1)]
        if d = 64 then
            res.Add((x,y)) |> ignore
        else
            for nx, ny in neighbors do
                if nx >= 0 && nx < Array2D.length2 map && ny >= 0 && ny < Array2D.length1 map then
                    let c = map[ny, nx]
                    if c <> '#' then
                        q ((nx, ny), d + 1)

    res.Count


[<AocSolver(2023, 21, Level = 2)>]
let solve2 (input: string list) =
    let smallMap = input |> array2D
    let sh = Array2D.length1 smallMap
    let sw = Array2D.length2 smallMap

    let map = Array2D.zeroCreate (Array2D.length1 smallMap * 9) (Array2D.length2 smallMap * 9)
    for y = 0 to Array2D.length1 map - 1 do
        for  x = 0 to Array2D.length2 map - 1 do
            map[y,x] <- smallMap[y % sh,x % sw]

    let S = (Array2D.length2 map / 2, Array2D.length1 map / 2)

(*
 For altered test input:
0/5: 26
1/16: 216
2/27: 588
3/38: 1142
4/49: 1878
*)

(*

let tri n = n * (n + 1) / 2
let sample = [ 26 216 588 ]
let diffs = [ 190 372 ]
let step = 182
let offset = 190 - 182 = 8


let totalSteps = 71
71 - 5 / 11 = 6
26 + (tri 6 * 182) + (6 * 8)

*)
    let mutable sample = []
    let distanceTilSide = Array2D.length1 smallMap / 2
    let width = Array2D.length2 smallMap

    for i in 0 .. 2 do
        let Q = System.Collections.Generic.Queue()
        let seen = System.Collections.Generic.HashSet()
        let q e =
            if not (seen.Contains(e)) then
                Q.Enqueue(e)
                seen.Add(e) |> ignore
        let res = System.Collections.Generic.HashSet()
        q (S, 0)
        while Q.Count > 0 do
            let (x, y), d = Q.Dequeue()
            let neighbors = [(x+1,y); (x-1,y); (x,y+1); (x,y-1)]
            if d = distanceTilSide + width * i then
                res.Add((x,y)) |> ignore
            else
                for nx, ny in neighbors do
                    if nx >= 0 && nx < Array2D.length2 map && ny >= 0 && ny < Array2D.length1 map then
                        let c = map[ny, nx]
                        if c <> '#' then
                            q ((nx, ny), d + 1)
        sample <- sample @ [res.Count]
        printfn $"{i}/{distanceTilSide + width * i}: {res.Count}"
    let diffs = List.pairwise sample |> List.map (fun (a, b) -> int64 (b - a))
    let diff = List.pairwise diffs |> List.map (fun (a, b) -> b - a) |> List.head
    let offset = List.head diffs - diff
    let totalSteps = 26501365L
    let n = (totalSteps - int64 distanceTilSide) / int64 width
    // let n = (totalSteps - 5) / 11
    let first = List.head sample |> int64

    let tri n = n * (n + 1L) / 2L
    first + (tri n * diff) + (n * offset)
