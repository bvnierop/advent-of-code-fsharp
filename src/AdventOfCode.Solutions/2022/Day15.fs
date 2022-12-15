namespace AdventOfCode.Solutions._2022

open AdventOfCode.Lib.Solver
open FParsec
open System

module Day15 =
    // Idea:
    //   parse each point
    //     "Sensor at x=2, y=18: closest beacon is at x=-2, y=15"
    //   quick method to calc manhattan distance
    //   Use that to calculate a range at line Y
    //   specifically: At precisely the manhattan distance, the range is 1
    //   At a larger distance the range is 0
    //   At a smaller distance, the range increases by 1 in each direction for each Y closer to the sensor
    //   When we have the ranges, compact them
    //     compacting a list of ranges means:
    //     - sort the list
    //     - start with the smallest range
    //     - compare it with the next one
    //     - if overlap -> merge
    //     - else: append current range to list of ranges, take next one as the new base
    //   Finally, sum the length of each range
    module Point =
        /// Calculates the manhattan distance between two points.
        let manhattanDistance (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)
        
    /// Determines the range for the given sensor at row `y`. The range of a sensor is
    /// at the Manhattan Distance from the sensor is `sensorX`. If `y` is closer to the sensor
    /// than the Manhattan Distance, the range is `sensorX - diff .. sensorX + diff`, inclusive.
    /// Further away, there is no range.
    let rangeForSensorAtRow y ((sensorX, sensorY), (beaconX, beaconY)) =
        let maxDistance = Point.manhattanDistance (sensorX, sensorY) (beaconX, beaconY)
        let distance = abs (sensorY - y)
        let diff = maxDistance - distance
        if diff < 0 then None
        else Some (sensorX - diff, sensorX + diff)
        
    module Range = 
        let compact (ranges: (int * int) seq) =
            let isDisjoint (range1: (int * int)) range2 =
                match (range1, range2) with
                | ((l1, h1), (l2, h2)) when h2 < l1 -> true
                | ((l1, h1), (l2, h2)) when h1 < l2 -> true
                | _ -> false
                
            let overlap (start1, finish1) (start2, finish2) =
                (not <| isDisjoint (start1, finish1) (start2, finish2))
                || start2 - finish1 = 1
                
            let merge (s1, e1) (s2, e2) = ((min s1 s2), (max e1 e2))
                
            let sortedRanges = ranges |> Seq.sort
            Seq.tail sortedRanges
            |> Seq.fold (fun (head, ranges) nextRange ->
                if overlap head nextRange then (merge head nextRange, ranges)
                else (nextRange, (head :: ranges))) (Seq.head sortedRanges, []) 
            |> (fun (head, ranges) -> head :: ranges)
            |> List.rev
            
        /// Length of a range, inclusive. So a range of (5, 5) has length 1.
        let length (start, finish) = finish - start + 1
        
    let pLine =
        (skipString "Sensor at x=" >>. pint32 .>> skipString ", y=" .>>. pint32)
        .>>. (skipString ": closest beacon is at x=" >>. pint32 .>> skipString ", y=" .>>. pint32)
        |> parseOrDie
        
    let solve y input =
        let area = input |> List.map pLine
        let beacons =
            area
            |> List.map snd
            |> List.distinct
        
        area
        |> List.map (rangeForSensorAtRow y)
        |> List.choose id
        |> Range.compact
        |> List.map Range.length
        |> List.sum
        |> (fun s -> s - (beacons |> List.filter (fun (_bx, by) -> by = y) |> List.length))
        
        
    [<AocSolver(2022, 15, Level = 1)>]
    let solve1 (input: string list) =
        if List.length input = 14 then solve 10 input
        else solve 2000000 input
        
    [<AocSolver(2022, 15, Level = 2)>]
    let solve2 (input: string list) =
        let maxY = if List.length input = 14 then 20 else 4000000
        let sensors = input |> List.map pLine
        {0..maxY}
        |> Seq.pick (fun y ->
            let ranges =
                sensors |> List.map (rangeForSensorAtRow y) |> List.choose id
                |> Range.compact
            if List.length ranges = 1 then None
            else Some (int64 y + 4000000L * int64 (ranges |> List.head |> snd |> ((+) 1))))
