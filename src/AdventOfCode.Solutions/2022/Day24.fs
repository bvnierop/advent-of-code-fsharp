namespace AdventOfCode.Solutions._2022

open System.Collections.Generic
open System.Text.Json
open AdventOfCode.Lib.Solver
open System
open AdventOfCode.Solutions._2022.Day23

module Math =
    let rec gcd a b =
        match (a,b) with
        | (x, 0) -> x
        | (0, y) -> y
        | (a, b) -> gcd b (a % b)
    
    let lcm a b = a * b / (gcd a b)
    
    let modE a b = ((a % b) + b) % b
    
// Basic solution: BFS
//  BFS State: (x, y, totalMinutesSpent
//  Seen State: (x, y, minutes % lcm sides)
// Read input:
//  - Put into 2D array
//  - Get lengths
//  - Get LCM
//  - Get start / end
//  - Find / map all blizzards
//     (x, y, direction)

// So solution:
//   Read input, store grid (not needed), blizzards, start, end
//   Do BFS. Neighbours are x+dx,y+dy. They're valid if blizzard + dx/dy % (side - 2) + 1 <> neighbour
//
//     For all blizzards we normalize their position.
//      So to calc a blizzard's X we calc ((X + (DX[Direction] * totalMinutes) - 1) % (side - 2)) + 1
//

module MutableHashMap =
    type t<'a,'b when 'a: equality> = Dictionary<'a, 'b>
    let empty<'a, 'b when 'a: equality> = new Dictionary<'a, 'b>()
    let add key value (dict: t<'a,'b>) = dict.Add(key, value); dict
    let containsKey key (dict: t<'a, 'b>) = dict.ContainsKey(key)

module Day24 =
    type Direction = East = 0 | South = 1 | West = 2 | North = 3
    let dx = [1; 0; -1; 0; 0]
    let dy = [0; 1; 0; -1; 0]
    
    type Valley = {
        Grid: char array array
        Blizzards: (int * int * Direction) list
        Width: int
        Height: int
    }
    
    let parseDirection =
        function
        | '>' -> Some Direction.East
        | 'v' -> Some Direction.South
        | '<' -> Some Direction.West
        | '^' -> Some Direction.North
        | _ -> None
    
    let parseBlizzards grid =
        let parsedPoints = seq {
            for y = 0 to Array.length grid - 1 do
                for x = 0 to Array.length grid[y] - 1 do
                    yield (x, y, parseDirection grid[y].[x])
        }
        parsedPoints
        |> Seq.filter (fun (_, _, d) -> Option.isSome d)
        |> Seq.map (fun (x, y, d) -> (x, y, Option.defaultValue (enum -1) d))
        |> Seq.toList
    
    let parseValley input =
        let asArray = input |> List.map Seq.toArray |> List.toArray
        let width = (Array.length asArray[0])
        let height = (Array.length asArray)
        let blizzards = parseBlizzards asArray
        { Grid = asArray; Width = width; Height = height; Blizzards = blizzards }
        
    let blizzardPosition time valley (x, y, dir) =
        // So to calc a blizzard's X we calc ((X + (DX[Direction] * totalMinutes) - 1) % (side - 2)) + 1
        //  This removes the walls from the position
        let height = valley.Height - 2
        let width = valley.Width - 2
        let curX = ((x - 1 + (dx[int dir] * time)) |> Math.modE <| width) + 1
        let curY = ((y - 1+ (dy[int dir] * time)) |> Math.modE <| height) + 1
        assert (curX > 0 && curX < valley.Width - 1 && curY > 0 && curY < valley.Height - 1)
        curX, curY
        
    let countBlizzards' x y time valley =
        valley.Blizzards
        |> List.map (blizzardPosition time valley)
        |> List.countWhere (fun (bx, by) -> x = bx && y = by)
        
    let inline encode x y t = (x <<< 20) ||| (y <<< 10) ||| t
        
    let memo3 f =
        let mutable cache = MutableHashMap.empty
        let memoized a b c =
            let key = encode a b c
            if not (MutableHashMap.containsKey key cache) then
                cache <- MutableHashMap.add key <| f a b c <| cache
            cache[key]
        memoized
        
    let countBlizzards = memo3 countBlizzards'
        
    let noBlizzard x y time valley =
        countBlizzards x y time valley = 0
        
    let printState (xx, yy, time) valley =
        for y = 0 to valley.Height - 1 do
            for x = 0 to valley.Width - 1 do
                let count = countBlizzards x y time valley
                if count > 0 then
                    if x = xx && y = yy then printf "X"
                    else printf $"{count}"
                else if x = xx && y = yy then printf "E"
                else if x = 0 || x = valley.Width - 1 || y = 0 || y = valley.Height - 1 then printf $"{valley.Grid[y].[x]}"
                else printf "."
            printfn ""
        printfn ""
        
        
    let search (startX, startY) startTime (finishX, finishY) valley =
        let start = (startX, startY, startTime)
        let cycle = Math.lcm (valley.Height - 2) (valley.Width - 2)
        
        printfn $"Width: {valley.Width}, Height: {valley.Height}, Cycle size: {cycle}"
        
        let rec loop queue seen =
            if Queue.isEmpty queue then seen, None // No answer found
            else
                let (tileX, tileY, tileTime) = Queue.front queue
                if tileY = finishY && tileX = finishX then seen, Some tileTime
                else
                    [|0..4|]
                    |> Array.fold (fun (q, s) d ->
                        let (nx, ny, nd) = (tileX + dx[d], tileY + dy[d], tileTime + 1)
                        if nx >= 0 && nx < valley.Width && ny >= 0 && ny < valley.Height
                           && valley.Grid[ny].[nx] <> '#' && not (HashSet.contains (encode nx ny (nd % cycle)) seen)
                           && noBlizzard nx ny nd valley then
                            Queue.enqueue (nx, ny, nd) q, HashSet.add (encode nx ny (nd % cycle)) s
                        else q, s) (Queue.dequeue queue, seen)
                    ||> loop
        let (seen, res) = loop <| Queue.enqueue start Queue.empty <| HashSet.empty
        printfn $"Finished search. Seen {HashSet.size seen} different states."
        res
        
        
                    
    [<AocSolver(2022, 24, Level = 1)>]
    let solve1 (input: string list) =
        let valley = parseValley input
        let start = (1, 0)
        let finish = (valley.Width - 2, valley.Height - 1)
        search start 0 finish valley |> Option.defaultValue -1
    
        
    [<AocSolver(2022, 24, Level = 2)>]
    let solve2 (input: string list) =
        let valley = parseValley input
        let start = (1, 0)
        let finish = (valley.Width - 2, valley.Height - 1)
        let timeToFinish = search start 0 finish valley |> Option.defaultValue -1
        let timeToStart = search finish timeToFinish start valley |> Option.defaultValue -1
        let timeToFinishAgain = search start timeToStart finish valley |> Option.defaultValue -1
        timeToFinishAgain
        
