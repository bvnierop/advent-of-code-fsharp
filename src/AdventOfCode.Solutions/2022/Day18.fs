namespace AdventOfCode.Solutions._2022

open AdventOfCode.Lib.Solver
open System
open FParsec

module PackedPoint =
    let make x y z =
        (x <<< 16) ||| (y <<< 8) ||| z
        
    let z pack = pack &&& 0x0000FF
    let y pack = (pack &&& 0x00FF00) >>> 8
    let x pack = (pack &&& 0xFF0000) >>> 16
    
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
        let lava = input |> List.map parseLine |> List.map (fun (x, y, z) -> PackedPoint.make (x + 1) (y + 1) (z + 1)) |> Set.ofList
        
        let neighbours packed = [
            let x = PackedPoint.x packed
            let y = PackedPoint.y packed
            let z = PackedPoint.z packed
            
            PackedPoint.make (x - 1) y z; PackedPoint.make (x + 1) y z;
            PackedPoint.make x (y - 1) z; PackedPoint.make x (y + 1) z;
            PackedPoint.make x y (z - 1); PackedPoint.make x y (z + 1)
        ]
        
        let neighboursInRange packedPoint =
            packedPoint |> neighbours
            |> List.filter (fun packedPoint ->
                let x = PackedPoint.x packedPoint
                let y = PackedPoint.y packedPoint
                let z = PackedPoint.z packedPoint
                0 <= x && x <= 24 && 0 <= y && y <= 24 && 0 <= z && z <= 24)
            
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
            
        floodFill (PackedPoint.make 0 0 0) |> fst
