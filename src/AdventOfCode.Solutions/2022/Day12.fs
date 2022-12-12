namespace AdventOfCode.Solutions._2022

open AdventOfCode.Lib.Solver
open System

module Day12 =
    let rec parseChr = function
        | 'S' -> parseChr 'a'
        | 'E' -> parseChr 'z'
        | c -> Convert.ToInt32(c) - Convert.ToInt32('a')
        
    let parseLine line = line |> Array.map parseChr
    let parse (input: string list) =
        let asArray = input |> List.map Seq.toArray |> List.toArray
        (Array.findIndex2D ((=) 'S') asArray, Array.findIndex2D ((=) 'E') asArray,
         asArray |> Array.map parseLine)
    
    let bfs start finishFn validFn (grid: 'a array array) =
        let rec loop q seen =
            if Queue.isEmpty q then None // No answer found
            else
                let (dist, (p1, p2)) = Queue.front q
                if finishFn ((p1, p2), grid.[p1].[p2]) then Some dist
                else
                    let (newQ, newSeen) =
                        Seq.fold (fun (newQ, newSeen) npos ->
                            if not <| Set.contains npos newSeen && validFn ((p1, p2), grid.[p1].[p2]) (npos, grid.[fst npos].[snd npos]) then
                                (Queue.enqueue (dist + 1, npos) newQ,
                                Set.add npos newSeen)
                            else
                                (newQ, newSeen)) <| (Queue.dequeue q, seen) <| (Array.neighbours <| p1 <| p2 <| grid)
                    loop newQ newSeen
                    
        let q = Queue.enqueue (0, start) (Queue.empty ())
        let seen = Set.add start Set.empty
        loop q seen

    [<AocSolver(2022, 12, Level = 1)>]
    let solve1 (input: string list) =
        let (start, finish, grid) = parse input
        
        bfs start (fun (pos, _) -> pos = finish)
                   (fun (_, h) (_, h2) -> h2 - h <= 1)
                   grid
        |> Option.defaultValue -1
        
    [<AocSolver(2022, 12, Level = 2)>]
    let solve2 (input: string list) =
        let (start, finish, grid) = parse input
        
        bfs finish (fun (_, h) -> h = 0)
                   (fun (_, h2) (_, h) -> h2 - h <= 1)
                   grid
        |> Option.defaultValue -1