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

        match (dx, dy) with
        | (0, 0) | (0, 1) | (0, -1) | (1, 0) | (-1, 0)
        | (1, 1) | (1, -1) | (-1, 1) | (-1, -1) -> (tx, ty)

        | (0, 2) | (1, 2) | (-1, 2) -> (hx, hy - 1)
        | (0, -2) | (1, -2) | (-1, -2) -> (hx, hy + 1)

        | (2, 0) | (2, 1) | (2, -1) -> (hx - 1, hy)
        | (-2, 0) | (-2, 1) | (-2, -1) -> (hx + 1, hy)

        | (2, 2) -> (hx - 1, hy - 1)
        | (-2, 2) -> (hx + 1, hy - 1)
        | (2, -2) -> (hx - 1, hy + 1)
        | (-2, -2) -> (hx + 1, hy + 1)

        | _ -> failwith $"Failed to update: (({hx}, {hy}) - ({tx}, {ty}) -> {dx}, {dy})"

    let solveForLength n input =
        let mutable visited = Set.empty
        let mutable rope = List.init n (fun _ -> (0, 0))
        //let mutable head = (0, 0)
        //let mutable tail = (0, 0)
        for line in input do
            let moves = parseLine line
            for (dx, dy) in moves do
                //head <- (fst head + dx, snd head + dy)
                //tail <- updateTail head tail
                //visited <- Set.add tail visited

                let (h::t) = rope
                let nh = (fst h + dx, snd h + dy)
                rope <- List.scan updateTail nh t
                visited <- Set.add (List.last rope) visited
        Seq.length visited
    
    [<AocSolver(2022, 9, Level = 1)>]
    let solve1 (input: string list) =
        solveForLength 2 input
        
    [<AocSolver(2022, 9, Level = 2)>]
    let solve2 (input: string list) =
        solveForLength 10 input