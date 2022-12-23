namespace AdventOfCode.Solutions._2022

open AdventOfCode.Lib.Solver
open System


module Day23 =
    let toElves input =
        let asArray = input |> List.map Seq.toArray |> List.toArray
        [
            for y = 0 to asArray.Length - 1 do
                for x = 0 to asArray[0].Length - 1 do
                    if asArray[y][x] = '#' then yield (x, y)
        ] |> HashSet.ofSeq
        
    type Direction = {
        Dest: int * int
        MustBeEmpty: (int * int) array
    }
    /// Initial directions are north, south, west, east
    /// They also contain the checks that must be made
    let initialDirections = [
        { Dest = (0, -1); MustBeEmpty = [|(0, -1); (-1, -1); (1, -1)|] }
        { Dest = (0, 1); MustBeEmpty = [| (0, 1); (-1, 1); (1, 1) |] }
        { Dest = (-1, 0); MustBeEmpty = [| (-1, 0); (-1, -1); (-1, 1) |] }
        { Dest = (1, 0); MustBeEmpty = [| (1, 0); (1, -1); (1, 1) |] }
    ]
    
    let neighbours = [| for x = -1 to 1 do for y = -1 to 1 do if x <> 0 || y <> 0 then yield (x, y) |]
    
    /// An elf wants to move if one of the adjacent tiles contains an elf.
    let wantsToMove (elfX, elfY) elves =
        neighbours
        |> Array.exists (fun (nX, nY) -> HashSet.contains (elfX + nX, elfY + nY) elves)
        
    /// Determines if a list of given points is unoccupied
    let unoccupied elves points =
        points
        |> Seq.exists (fun (x, y) -> HashSet.contains (x, y) elves)
        |> not
        
    /// Converts a combination of point + offset to a new point
    let offsetToPoint (elfX, elfY) (dx, dy) = (elfX + dx, elfY + dy)
    
    /// Consider the next position for the given elf
    ///  The next consideration for the elf is the first free position
    ///  If none is free it stays put. This, too, is a consideration.
    ///  An elf only moves if it's adjacent to another elf
    /// Returns (currentPosition, wantedDestination)
    let makeConsideration directions elves elf =
        let dst =
            if wantsToMove elf elves then
                [0..(CircularCollection.length directions) - 1]
                |> Seq.map (fun i -> // Map to potential offsets. Return None if no direction is free
                    let dir = CircularCollection.itemAt i directions
                    let isFree = dir.MustBeEmpty // offsets that must be empty for this direction
                                 |> Array.map (offsetToPoint elf)  // points that must be empty for this direction
                                 |> unoccupied elves // is free
                    if isFree then Some (offsetToPoint elf dir.Dest)
                    else None)
                |> Seq.tryPick id // Pick the first free direction
                |> Option.defaultValue elf // If none is free, don't move
            else elf
        (elf, dst)
        
    /// Calculates considerations. Returns both the considerations and counts for each destination.
    let makeConsiderations directions elves = elves |> Seq.map (makeConsideration directions elves) |> Seq.toList
        
    /// Perform as many moves as possible.
    /// A Move is (src -> dst). It's possible if only one elf wants to move there.
    /// IF a move is made, src has to be removed from the set of elves and dst has to be added
    let performMoves elves considerations =
        let counts = considerations |> Seq.countBy snd |> Map.ofSeq
        let rec loop remaining movedElves elvesMovedCount =
            match remaining with
            | [] -> movedElves, elvesMovedCount
            | (src,dst)::xs ->
                if src <> dst && counts[dst] = 1 then loop xs (movedElves |> HashSet.remove src |> HashSet.add dst) (elvesMovedCount + 1)
                else loop xs movedElves elvesMovedCount
        loop considerations elves 0
    
    /// Simulates a single round. Returns the new elves locations and the new order of directions.
    let round directions elves =
        // For each step we first make considerations, and then perform them
        let considerations = makeConsiderations directions elves
        let movedElves, elvesMovedCount = performMoves elves considerations
        (CircularCollection.moveNext directions, movedElves, elvesMovedCount)
        
    let rect elves =
        let minX = elves |> Seq.minBy fst |> fst
        let maxX = elves |> Seq.maxBy fst |> fst
        let minY = elves |> Seq.minBy snd |> snd
        let maxY = elves |> Seq.maxBy snd |> snd
        (maxX - minX + 1) * (maxY - minY + 1)
    
    [<AocSolver(2022, 23, Level = 1)>]
    let solve1 (input: string list) =
        let elves = input |> toElves
        {1..10}
        |> Seq.scan (fun (directions, elves, count) _i -> round directions elves) (CircularCollection.init initialDirections, elves, -1)
        |> Seq.last
        |> (fun (_, e, _) -> e)
        |> rect
        |> (fun r -> r - HashSet.size elves)
        
    [<AocSolver(2022, 23, Level = 2)>]
    let solve2 (input: string list) =
        let elves = input |> toElves
        {1..1000}
        |> Seq.scan (fun (directions, elves, count) _i -> round directions elves) (CircularCollection.init initialDirections, elves, -1)
        |> Seq.findIndex (fun (_, _, count) -> count = 0)
