namespace AdventOfCode.Solutions._2022

open AdventOfCode.Lib.Solver
open System
open FParsec

module Day18 =
    let pLine = pint32 .>> pchar ',' .>>. pint32 .>> pchar ',' .>>. pint32 |>> (fun ((x, y), z) -> (x, y, z))
    let parseLine = parseOrDie pLine
    
    let isConnected (x1, y1, z1) (x2, y2, z2) =
        (abs (x1 - x2)) + (abs (y1 - y2)) + (abs (z1 - z2)) = 1
    
    [<AocSolver(2022, 18, Level = 1)>]
    let solve1 (input: string list) =
        let blockedSides =
            input
            |> List.map parseLine
            |> List.pairs
            |> Seq.filter (fun (p1, p2) -> isConnected p1 p2)
            |> Seq.length
            
        List.length input * 6 - blockedSides * 2
        
    [<AocSolver(2022, 18, Level = 2)>]
    let solve2 (input: string list) =
        let lava = input |> List.map parseLine |> Set.ofList
        
        let neighbours (x, y, z) = [
            (x - 1, y, z); (x + 1, y, z);
            (x, y - 1, z); (x, y + 1, z)
            (x, y, z - 1); (x, y, z + 1)
        ]
        
        let neighboursInRange point =
            point |> neighbours
            |> List.filter (fun (x, y, z) -> -1 <= x && x <= 24 && -1 <= y && y <= 24 && -1 <= z && z <= 24)
            
        let floodFill start =
            let seen = Set.empty |> Set.add start
            let rec loop at cellsSeen lavaSeenCount =
                let neighbours = neighboursInRange at
                let air = neighbours |> List.reject (fun pt -> Set.contains pt lava)
                let lavaCount = List.length neighbours - List.length air
                
                let (lavaSeenAtNeighbours, cellsSeenAtNeighbours) =
                    List.fold (fun (lavaSeenCount, cellsSeen) nextCell ->
                        if Set.contains nextCell cellsSeen then (lavaSeenCount, cellsSeen)
                        else loop nextCell <| Set.add nextCell cellsSeen <| lavaSeenCount) (lavaSeenCount + lavaCount, cellsSeen) air
                
                (lavaSeenAtNeighbours, cellsSeenAtNeighbours)
                    
            loop start seen 0
            
        floodFill (-1, -1, -1) |> fst
