namespace AdventOfCode.Solutions._2022

open System.Collections.Immutable
open AdventOfCode.Lib.Solver
open FParsec
open System

module Day14 =
    let pPoint = pint32 .>> pchar ',' .>>. pint32
    let pLine = sepBy pPoint (pstring " -> ")
    let parse str = parseOrDie pLine str
    
    let rec addSand (rocksA: bool[,]) threshold (atX, atY) =
        let targets = [| [|atX; atY + 1|]; [|atX - 1; atY + 1|]; [|atX + 1; atY + 1|] |]
        if atY >= threshold || rocksA[500,0] then false
        else
            let mutable found = false
            let mutable i = 0
            while not found && i < 3 do
                found <- not rocksA[targets[i][0],targets[i][1]]
                if not found then i <- i + 1
                
            if found then addSand rocksA threshold (targets[i][0],targets[i][1])
            else Array2D.set rocksA atX atY true; true
                
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
        
        let rocksA = Array2D.initBased -100 0 1201 200 (fun i j -> false)
        for (x, y) in preppedRocks do rocksA[x,y] <- true
            
            
        let low = preppedRocks |> Seq.maxBy snd |> snd
        let rec simulate count =
            let added = addSand rocksA low (500, 0)
            if not added then count
            else simulate <| count + 1
            
        simulate 0
        
    [<AocSolver(2022, 14, Level = 1)>]
    let solve1 (input: string list) =
        solver input id
        
    [<AocSolver(2022, 14, Level = 2)>]
    let solve2 (input: string list) =
        let modify rocks =
            let low = rocks |> HashSet.toSeq |> Seq.maxBy snd |> snd
            [-100..1100] |> List.fold (fun occ x -> HashSet.add (x, low + 2) occ) rocks
            
        solver input modify