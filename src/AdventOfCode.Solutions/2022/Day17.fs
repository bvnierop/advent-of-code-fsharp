namespace AdventOfCode.Solutions._2022

open System.Drawing
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
        
module FastCycle =
    type t<'a> = ('a array * int * int)
    
    let init (source: 'a seq) =
        let asArray = Seq.toArray source
        (asArray, 0, Array.length asArray)
        
    let moveNext ((source, index, length): t<'a>) =
        let nextIndex = (index + 1) % length
        (source, nextIndex, length)
        
    let item ((source, index, _length): t<'a>) =
        source[index]
        
module Day17 =
    // We'll model both the rocks and the chamber by (lists of) integers.
    // We're specifically interest in the binary representation of these integers.
    // This makes it easy to check for collision. If X & Y != 0, then there's a
    // collision.
    //
    // We use a custom library function, Seq.cyclic, for both cyclic lists.
    
    // Simulating, a newly spawned rock first performs the next four shifts.
    // Then, it falls (if possible). Then it starts alternating shifting and falling.
    // If it can no longer fall, it's done.
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
            
        let simulate rocks jets =
            // grab a rock and spawn it
            // apply the next four jet patterns
            // then start alternating dropping and applying a jet pattern
            // drop: overlap the tops with &&&. If the result is 0b100000001 then
            //       we can indeed drop
            // apply jet pattern:
            //   bitshift _in the other direction_ since we're doing everything in reverse
            //   and verify.
            //   For the start we only have to verify against walls
            //   That's easy: rock &&& 0b100000001 must be 0
            //   To be fair, that's how we can always shift. shiftedRock &&& targetMask must be 0 for a shift to be possible
            //
            // We should probably just always simulate every step.
            //  The current playing field should be line that has a rock in it OR the floor.
            //  To spawn a rock we'll append three new lines to the playing field, plus the size of the rock.
            //  And the rock itself
            //
            // So we have
            let chamber = [floor]
            // A step consists of applying a jet and falling down
            //  first apply the jet
            let applyJet jet rock =
                let fn = match jet with | '<' -> (>>>) | '>' -> (<<<) | _ -> failwith "Invalid jet"
                rock |> List.map (fun x -> fn x 1)
                
            let isValidPosition rock chamber =
                Seq.forall2 (fun r c -> r &&& c = 0) <| rock <| ListZipper.view chamber
                
            // In order to simulate we need to represent the chamber as a zipper
            let chamberZipper = ListZipper.init chamber
            let tryFall chamber rock =
                let nextChamberState = ListZipper.next chamber // should always work because we have a floor
                if isValidPosition rock nextChamberState then Some nextChamberState
                else None
                
            let tryJet chamber jet rock =
                let shiftedRock = applyJet jet rock
                if isValidPosition shiftedRock chamber then Some shiftedRock
                else None
                
            let tryStep chamber rock jets =
                let shiftedRock = Option.defaultValue <| rock <| tryJet chamber (FastCycle.item jets) rock
                let fallen = tryFall chamber <| shiftedRock
                (Option.defaultValue chamber fallen, shiftedRock, FastCycle.moveNext jets, Option.isSome fallen)
                
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
                    // printProgress chamber rock
                    let (newChamber, newRock, newJets, success) = tryStep chamber rock jets
                    if success then doSimulate newChamber newRock newJets
                    else (merge chamber newRock, FastCycle.moveNext rocks, newJets)
                let (rock, chamber) = spawn (FastCycle.item rocks) chamber
                doSimulate <| ListZipper.init chamber <| rock <| jets
                
            let rockCycle = FastCycle.init rocks
            let jetCycle = FastCycle.init jets
            
            // let (c1, r1, j1) = simulateOneRock chamber rockCycle jetCycle
            // let (c2, r2, j2) = simulateOneRock c1 r1 j1
            // print c1
            // print c2
            
            let simulateMultipleRocks chamber rocks jets n =
                [1..n]
                |> List.fold (fun (c, r, j) _i -> simulateOneRock c r j) (chamber, rocks, jets)
                    
            simulateMultipleRocks chamber rockCycle jetCycle 2022
            |> (fun (c, _, _) -> List.length c - 1)
        
    [<AocSolver(2022, 17, Level = 1)>]
    let solve1 (input: string) =
        Tetris.simulate Tetris.rocks (input.Trim())
        
    [<AocSolver(2022, 17, Level = 2)>]
    let solve2 (input: string) =
        2