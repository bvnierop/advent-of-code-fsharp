namespace AdventOfCode.Solutions._2022

open AdventOfCode.Lib.Solver
open FParsec
open System

module SetWithCount =
    type t<'a when 'a: comparison> = Set<'a> * int
    
    let add item ((set, count): t<'a>) =
        if Set.contains item set then (set, count)
        else (Set.add item set, count + 1)
        
    let contains item ((set, _count): t<'a>) = Set.contains item set
        
    let size ((_set, count): t<'a>) = count
    
    let empty<'a> = (Set.empty, 0)
    
    let ofSeq seq = seq |> Seq.fold (fun s e -> add e s) empty
    let toSeq ((set, _count): t<'a>) = set

module Day14 =
    let pPoint = pint32 .>> pchar ',' .>>. pint32
    let pLine = sepBy pPoint (pstring " -> ")
    let parse str = parseOrDie pLine str
    
    [<AocSolver(2022, 14, Level = 1)>]
    let solve1 (input: string list) =
        let points ((x1, y1), (x2, y2)) = seq {
            for x = min x1 x2 to max x1 x2 do
                for y = min y1 y2 to max y1 y2 do
                    yield (x, y)
        }
        
        let rocks =
            input |> List.map parse
            |> List.map (List.pairwise >> List.map points >> Seq.concat)
            |> Seq.concat
            |> Set.ofSeq
            
        let low = rocks |> Seq.maxBy snd |> snd
        
        let rec addSand occupied threshold (atX, atY) =
            let targets = [(atX, atY + 1); (atX - 1, atY + 1); (atX + 1, atY + 1)]
            if atY >= threshold then occupied
            else
                let target =
                    targets |> List.map (fun pt -> (pt, Set.contains pt occupied))
                    |> List.tryFind (fun (pt, occ) -> occ = false)
                match target with
                | Some (pt, _) -> addSand occupied threshold pt
                | None -> Set.add (atX, atY) occupied
            
        let rec simulate occupied =
            let withSand = addSand occupied low (500, 0)
            if Set.count occupied = Set.count withSand then occupied
            else simulate withSand
            
        let withSand = simulate rocks
        Set.count withSand - Set.count rocks
        
        
        
    [<AocSolver(2022, 14, Level = 2)>]
    let solve2 (input: string list) =
        let points ((x1, y1), (x2, y2)) = seq {
            for x = min x1 x2 to max x1 x2 do
                for y = min y1 y2 to max y1 y2 do
                    yield (x, y)
        }
        
        let rocks =
            input |> List.map parse
            |> List.map (List.pairwise >> List.map points >> Seq.concat)
            |> Seq.concat
            |> SetWithCount.ofSeq
            
        let low = rocks |> SetWithCount.toSeq |> Seq.maxBy snd |> snd
        
        let rocksWithLowerLayer = [-100..1100] |> List.fold (fun occ x -> SetWithCount.add (x, low + 2) occ) rocks
        
        let rec addSand occupied threshold (atX, atY) =
            let targets = [(atX, atY + 1); (atX - 1, atY + 1); (atX + 1, atY + 1)]
            if atY >= threshold then occupied
            else
                let target =
                    targets |> Seq.map (fun pt -> (pt, SetWithCount.contains pt occupied))
                    |> Seq.tryFind (fun (pt, occ) -> occ = false)
                match target with
                | Some (pt, _) -> addSand occupied threshold pt
                | None -> SetWithCount.add (atX, atY) occupied
            
        let rec simulate occupied =
            let withSand = addSand occupied (low + 2) (500, 0)
            if SetWithCount.size occupied = SetWithCount.size withSand then occupied
            else simulate withSand
            
        let withSand = simulate rocksWithLowerLayer
        SetWithCount.size withSand - SetWithCount.size rocksWithLowerLayer