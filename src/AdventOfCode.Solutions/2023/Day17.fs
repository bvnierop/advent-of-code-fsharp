module AdventOfCode.Solutions._2023.Day17

open System.Collections.Immutable
open AdventOfCode.Lib.Solver

(*
Because it is difficult to keep the top-heavy crucible going in a straight line for
very long, it can move at most three blocks in a single direction before it must
turn 90 degrees left or right. The crucible also can't reverse direction; after
entering each city block, it may only turn left, continue straight, or turn right.
*)

let dx = [| 0; 1; 0; -1 |]
let dy = [| -1; 0; 1; 0 |]

let up = 0
let right = 1
let down = 2
let left = 3

let inline turnLeft dir = (dir + 3) % 4
let inline turnRight dir = (dir + 1) % 4

type PQ = ImmutableSortedSet<int * (int * int) * int * int>
type Seen = ImmutableHashSet<(int * int) * int * int>

[<AocSolver(2023, 17, Level = 1)>]
let solve1 (input: string list) =
    let map = input |> array2D |> Array2D.map Int32.parseChr

    let Q = ImmutableSortedSet.Empty
    let Q = Q.Add((0, (0, 0), down, 0))
    let S = ImmutableHashSet.Empty
    let rec simulate (Q: PQ) (S: Seen) target =
        let cur = Q.Min
        let cost, (x, y), dir, times = cur
        let Q = Q.Remove(cur)
        if (x, y) = target then cost
        elif S.Contains ((x, y), dir, times)  then simulate Q S target
        else
            let S = S.Add ((x, y), dir, times)
            let next =
                [(x + dx[turnLeft dir], y + dy[turnLeft dir], turnLeft dir, 1);
                 if times < 3 then (x + dx[dir], y + dy[dir], dir, times + 1)
                 (x + dx[turnRight dir], y + dy[turnRight dir], turnRight dir, 1)]
                |> List.filter (fun (x, y, _, _) -> x >= 0 && x < Array2D.length2 map && y >= 0 && y < Array2D.length1 map) // in bounds
                |> List.reject (fun (x, y, dir, times) -> S.Contains ((x, y), dir, times))
            let Q = next |> List.fold (fun (Q: PQ) (x, y, dir, times) -> Q.Add((cost + map[y, x], (x, y), dir, times))) Q
            simulate Q S target
    simulate Q S (Array2D.length2 map - 1, Array2D.length1 map - 1)

[<AocSolver(2023, 17, Level = 2)>]
let solve2 (input: string list) =
    let map = input |> array2D |> Array2D.map Int32.parseChr

    let Q = ImmutableSortedSet.Empty
    let Q = Q.Add((0, (0, 0), down, 0))
    let Q = Q.Add((0, (0, 0), right, 0))
    let S = ImmutableHashSet.Empty
    let rec simulate (Q: PQ) (S: Seen) target =
        if Q.IsEmpty then failwith "No path found"
        let cur = Q.Min
        let cost, (x, y), dir, times = cur
        let Q = Q.Remove(cur)
        if (x, y) = target && times >= 4 then cost
        elif S.Contains ((x, y), dir, times)  then simulate Q S target
        else
            let S = S.Add ((x, y), dir, times)
            let next =
                [if times >= 4 then (x + dx[turnLeft dir], y + dy[turnLeft dir], turnLeft dir, 1);
                 if times < 10 then (x + dx[dir], y + dy[dir], dir, times + 1)
                 if times >= 4 then (x + dx[turnRight dir], y + dy[turnRight dir], turnRight dir, 1)]
                |> List.filter (fun (x, y, _, _) -> x >= 0 && x < Array2D.length2 map && y >= 0 && y < Array2D.length1 map) // in bounds
                |> List.reject (fun (x, y, dir, times) -> S.Contains ((x, y), dir, times))
            let Q = next |> List.fold (fun (Q: PQ) (x, y, dir, times) -> Q.Add((cost + map[y, x], (x, y), dir, times))) Q
            simulate Q S target
    simulate Q S (Array2D.length2 map - 1, Array2D.length1 map - 1)
