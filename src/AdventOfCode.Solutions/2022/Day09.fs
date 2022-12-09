namespace AdventOfCode.Solutions._2022

open AdventOfCode.Lib.Solver
open System

module Day09 =
    let parseLine (line: string) =
        match line.Split() with
        | [|"U"; n|] -> Seq.init (n |> Int32.parse) (fun _ -> (0, 1))
        | [|"D"; n|] -> Seq.init (n |> Int32.parse) (fun _ -> (0, -1))
        | [|"L"; n|] -> Seq.init (n |> Int32.parse) (fun _ -> (-1, 0))
        | [|"R"; n|] -> Seq.init (n |> Int32.parse) (fun _ -> (1, 0))
        | _ -> failwith "Failed to parse line"

    let updateTail (hx, hy) (tx, ty) =
        let dx = hx - tx
        let dy = hy - ty
        if abs dx > 1 || abs dy > 1 then
            (tx + sign dx, ty + sign dy)
        else
            (tx, ty)

    let solveForLength n input =
        let rope = List.init n (fun _ -> (0, 0))
        let executeMove rope (dx, dy) =
            match rope with
            | (hx, hy)::tail -> List.scan updateTail (hx + dx, hy + dy) tail
            | _ -> rope

        input
        |> List.map parseLine |> Seq.concat
        |> Seq.scan executeMove rope
        |> Seq.map List.last
        |> Set.ofSeq |> Set.length
    
    [<AocSolver(2022, 9, Level = 1)>]
    let solve1 (input: string list) =
        solveForLength 2 input
        
    [<AocSolver(2022, 9, Level = 2)>]
    let solve2 (input: string list) =
        solveForLength 10 input