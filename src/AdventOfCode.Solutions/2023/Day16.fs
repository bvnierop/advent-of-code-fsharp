module AdventOfCode.Solutions._2023.Day16

open AdventOfCode.Lib.Solver
open System

(*
The beam enters in the top-left corner from the left and heading to the right. Then, its behavior depends on what it encounters as it moves:

    If the beam encounters empty space (.), it continues in the same direction.
    If the beam encounters a mirror (/ or \), the beam is reflected 90 degrees depending on the angle of the mirror. For instance, a rightward-moving beam that encounters a / mirror would continue upward in the mirror's column, while a rightward-moving beam that encounters a \ mirror would continue downward from the mirror's column.
    If the beam encounters the pointy end of a splitter (| or -), the beam passes through the splitter as if the splitter were empty space. For instance, a rightward-moving beam that encounters a - splitter would continue in the same direction.
    If the beam encounters the flat side of a splitter (| or -), the beam is split into two beams going in each of the two directions the splitter's pointy ends are pointing. For instance, a rightward-moving beam that encounters a | splitter would split into two beams: one that continues upward from the splitter's column and one that continues downward from the splitter's column.
*)

let dx = [| 1; 0; -1; 0 |]
let dy = [| 0; 1; 0; -1 |]
let up = 3
let down = 1
let right = 0
let left = 2

let nextKeys (map: char array2d) nextX nextY dir =
    let next = map[nextY, nextX]
    match next with
    | '/' when dir = up -> [(nextX, nextY), right]
    | '/' when dir = down -> [(nextX, nextY), left]
    | '/' when dir = right -> [(nextX, nextY), up]
    | '/' when dir = left -> [(nextX, nextY), down]
    | '\\' when dir = up -> [(nextX, nextY), left]
    | '\\' when dir = down -> [(nextX, nextY), right]
    | '\\' when dir = right -> [(nextX, nextY), down]
    | '\\' when dir = left -> [(nextX, nextY), up]
    | '|' when dir = up || dir = down -> [(nextX, nextY), dir]
    | '|' when dir = right || dir = left -> [(nextX, nextY), up; (nextX, nextY), down]
    | '-' when dir = right || dir = left -> [(nextX, nextY), dir]
    | '-' when dir = up || dir = down -> [(nextX, nextY), left; (nextX, nextY), right]
    | _ -> [(nextX, nextY), dir]

let rec simulate Q S E map =
    if Queue.isEmpty Q then E
    else
        let (x, y), dir = Queue.front Q
        let key = Queue.front Q
        let Q = Queue.dequeue Q
        let E = Set.add (x, y) E
        let nextX, nextY = (x + dx[dir], y + dy[dir])
        if nextY >= 0 && nextY < Array2D.length1 map && nextX >= 0 && nextX < Array2D.length2 map then
            let nextKeys = nextKeys map nextX nextY dir
            let Q = nextKeys |> List.fold (fun q k -> if Set.contains k S then q else Queue.enqueue k q) Q
            let S = nextKeys |> List.fold (fun s k -> Set.add k s) S
            simulate Q S E map
        else simulate Q S E map

[<AocSolver(2023, 16, Level = 1)>]
let solve1 (input: string list) =
    let map = input |> array2D

    let startCoord = (0, 0, right)
    let start = startCoord |||> nextKeys map
    let Q = start |> List.fold (fun q k -> Queue.enqueue k q) Queue.empty
    let S = start |> List.fold (fun s k -> Set.add k s) Set.empty
    simulate Q S Set.empty map
    |> Set.length

[<AocSolver(2023, 16, Level = 2)>]
let solve2 (input: string list) =
    let map = input |> array2D

    let height = Array2D.length1 map
    let width = Array2D.length2 map
    let starts =
        ([0..height-1] |> List.collect (fun y -> [(0, y, right); (width - 1, y, left)]))
        @
        ([0..width-1] |> List.collect (fun x -> [(x, 0, down); (x, height - 1, up)]))

    List.map (fun startCoord ->
        let start = startCoord |||> nextKeys map
        let Q = start |> List.fold (fun q k -> Queue.enqueue k q) Queue.empty
        let S = start |> List.fold (fun s k -> Set.add k s) Set.empty
        simulate Q S Set.empty map
        |> Set.length
    ) starts
    |> List.max
