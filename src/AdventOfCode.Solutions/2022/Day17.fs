namespace AdventOfCode.Solutions._2022

open AdventOfCode.Lib.Solver
open System

module ListZipper =
    let init (list: 'a list): ('a list * 'a list) = ([], list)
    let next (head, tail) =
        match tail with
        | x::xs -> x :: head, xs
        | [] -> failwith "Cannot zip next on empty list"
        
    let prev (head, tail) =
        match head with
        | x::xs -> xs, x :: tail
        | [] -> failwith "Cannot zip prev on empty list"
        
    let update (updater: 'a list -> 'a list) (head, tail): ('a list * 'a list) = (head, updater tail)
    
    let hasNext (_head, tail) =
        match tail with | _x::_xs -> true | _ -> false
        
    let hasPrev (head, _tail) =
        match head with | _x::_xs -> true | _ -> false
        
    let view (_head, tail) = tail
    
    let rec rebuild (head, tail) =
        match head with
        | _x::_xs -> rebuild <| prev (head, tail)
        | [] -> tail
        
module CircularCollection =
    type t<'a> = ('a array * int * int)
    
    let init (source: 'a seq) =
        let asArray = Seq.toArray source
        (asArray, 0, Array.length asArray)
        
    let moveNext ((source, index, length): t<'a>) =
        let nextIndex = (index + 1) % length
        (source, nextIndex, length)
        
    let item ((source, index, _length): t<'a>) =
        source[index]
        
    let index ((_source, index, _length): t<'a>) = index
        
module Day17 =
    module Tetris =
        type Rock = int list
        type Chamber = int list
        let floor = 0b111111111
        let wall = 0b100000001
        
        let print chamber =
            for line in chamber do
                for tile = 0 to 8 do
                    if line &&& (1 <<< tile) = 0 then printf "."
                    else printf "#"
                printfn ""
            printfn ""
                
        let printProgress (head, tail) rock =
            for line in head |> List.rev do
                for tile = 0 to 8 do
                    if line &&& (1 <<< tile) = 0 then printf "."
                    else printf "#"
                printfn ""
                
            for c, r in Seq.zip tail rock do
                for tile = 0 to 8 do
                    if c &&& (1 <<< tile) <> 0 then printf "#"
                    elif r &&& (1 <<< tile) <> 0 then printf "@"
                    else printf "."
                printfn ""
                    
            for line in List.skip (List.length rock) tail do
                for tile = 0 to 8 do
                    if line &&& (1 <<< tile) = 0 then printf "."
                    else printf "#"
                printfn ""
            printfn ""
                
                
        let rocks = [
            [0b1111]
            [0b010
             0b111
             0b010]
            [0b100
             0b100
             0b111]
            [0b1
             0b1
             0b1
             0b1]
            [0b11
             0b11]
        ]
        
        let spawn rock chamber =
            let shiftedRock =
                rock
                |> List.map (fun line -> line <<< 3)
                
            let extendedChamber =
                rock
                |> List.fold (fun chamber rock -> wall :: chamber) chamber
            (shiftedRock, wall :: wall :: wall :: extendedChamber)
            
        let heightMap (chamber: int list) =
            let heightForIndex n =
                chamber |> List.findIndex (fun line -> line &&& (1 <<< n) <> 0)
            [7..-1..1]
            |> List.map heightForIndex
                         
        let simulate (steps: int64) rocks jets =
            let applyJet jet rock =
                let fn = match jet with | '<' -> (>>>) | '>' -> (<<<) | _ -> failwith "Invalid jet"
                rock |> List.map (fun x -> fn x 1)
                
            let isValidPosition rock chamber =
                Seq.forall2 (fun r c -> r &&& c = 0) <| rock <| ListZipper.view chamber
                
            let tryFall chamber rock =
                let nextChamberState = ListZipper.next chamber // should always work because we have a floor
                if isValidPosition rock nextChamberState then Some nextChamberState
                else None
                
            let tryJet chamber jet rock =
                let shiftedRock = applyJet jet rock
                if isValidPosition shiftedRock chamber then Some shiftedRock
                else None
                
            let tryStep chamber rock jets =
                let shiftedRock = Option.defaultValue <| rock <| tryJet chamber (CircularCollection.item jets) rock
                let fallen = tryFall chamber <| shiftedRock
                (Option.defaultValue chamber fallen, shiftedRock, CircularCollection.moveNext jets, Option.isSome fallen)
                
            let merge chamber rock =
                chamber
                |> ListZipper.update (fun tail ->
                        let top = (Seq.map2 (fun c r -> c ||| r) <| tail <| rock) |> Seq.toList
                        let rest = List.skip (List.length rock) tail
                        List.append top rest)
                |> ListZipper.rebuild
                |> List.reject ((=) wall)
                
            let simulateOneRock chamber rocks jets =
                let rec doSimulate chamber rock jets =
                    let (newChamber, newRock, newJets, success) = tryStep chamber rock jets
                    if success then doSimulate newChamber newRock newJets
                    else (merge chamber newRock, CircularCollection.moveNext rocks, newJets)
                let (rock, chamber) = spawn (CircularCollection.item rocks) chamber
                doSimulate <| ListZipper.init chamber <| rock <| jets
                
            let rockCycle = CircularCollection.init rocks
            let jetCycle = CircularCollection.init jets
            
            let simulateMultipleRocks chamber rocks jets n =
                {1..n}
                |> Seq.scan (fun (c, r, j) _i -> simulateOneRock c r j) (chamber, rocks, jets)
                    
            let emptyCache (): Map<(int list * int * int), (int * int)> = Map.empty
            let cacheKey chamber rocks jets = (heightMap chamber, CircularCollection.index rocks, CircularCollection.index jets)
            let inCache chamber rocks jets cache = Map.containsKey (cacheKey chamber rocks jets) cache
            let addToCache index chamber rocks jets cache = Map.add (cacheKey chamber rocks jets) (index, List.length chamber) cache
            let cacheValue chamber rocks jets (cache: Map<(int list * int * int), (int * int)>) = cache[cacheKey chamber rocks jets]
            
            let findCycle () =
                let rec loop cache index (chamber, rocks, jets) =
                    let (c, r, j) = simulateOneRock chamber rocks jets
                    if not (inCache c r j cache) then loop <| addToCache index c r j cache <| index + 1 <| (c, r, j)
                    else let curHeight = List.length c
                         let (cachedIndex, cachedHeight) = cacheValue c r j cache
                         (cachedIndex, index - cachedIndex, curHeight - cachedHeight)
                loop <| emptyCache () <| 1 <| ([floor], rockCycle, jetCycle)
                
            let (cStart, cLength, cHeight) = findCycle ()
            
            let pre = cStart
            let cycles = (steps - int64 cStart) / int64 cLength
            let post = int32 <| (steps - int64 cStart) % int64 cLength
            
            let heightFromCycles = cycles * int64 cHeight
            
            let smallSeq = pre + post
            simulateMultipleRocks [floor] rockCycle jetCycle smallSeq
            |> Seq.last
            |> (fun (c, _, _) -> int64 <| List.length c - 1)
            |> ((+) heightFromCycles)
        
    [<AocSolver(2022, 17, Level = 1)>]
    let solve1 (input: string) =
        Tetris.simulate 2022L Tetris.rocks (input.Trim())
        
    [<AocSolver(2022, 17, Level = 2)>]
    let solve2 (input: string) =
        Tetris.simulate 1000000000000L Tetris.rocks (input.Trim())