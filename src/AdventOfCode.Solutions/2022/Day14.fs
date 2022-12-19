namespace AdventOfCode.Solutions._2022

open System.Collections.Immutable
open AdventOfCode.Lib.Solver
open FParsec
open System

module Day14 =
    let pPoint = pint32 .>> pchar ',' .>>. pint32
    let pLine = sepBy pPoint (pstring " -> ")
    let parse str = parseOrDie pLine str
    
    let solver (input: string list) modifyFn =
        let points ((x1, y1), (x2, y2)) = seq {
            for x = min x1 x2 to max x1 x2 do
                for y = min y1 y2 to max y1 y2 do
                    yield (x, y)
        }
        
        let rocks =
            input |> List.map parse
            |> List.map (List.pairwise >> List.map points >> Seq.concat)
            |> Seq.concat
            |> HashSet.ofSeq
            
        let preppedRocks = modifyFn rocks
        
        let rec addSand occupied threshold (atX, atY) =
            let targets = [(atX, atY + 1); (atX - 1, atY + 1); (atX + 1, atY + 1)]
            if atY >= threshold then occupied
            else
                let target =
                    targets |> Seq.map (fun pt -> (pt, HashSet.contains pt occupied))
                    |> Seq.tryFind (fun (pt, occ) -> occ = false)
                match target with
                | Some (pt, _) -> addSand occupied threshold pt
                | None -> HashSet.add (atX, atY) occupied
            
        let low = preppedRocks |> Seq.maxBy snd |> snd
        let rec simulate occupied =
            let withSand = addSand occupied low (500, 0)
            if HashSet.size occupied = HashSet.size withSand then occupied
            else simulate withSand
            
        let withSand = simulate preppedRocks
        HashSet.size withSand - HashSet.size preppedRocks
        
    [<AocSolver(2022, 14, Level = 1)>]
    let solve1 (input: string list) =
        solver input id
        
    [<AocSolver(2022, 14, Level = 2)>]
    let solve2 (input: string list) =
        let modify rocks =
            let low = rocks |> HashSet.toSeq |> Seq.maxBy snd |> snd
            [-100..1100] |> List.fold (fun occ x -> HashSet.add (x, low + 2) occ) rocks
            
        solver input modify