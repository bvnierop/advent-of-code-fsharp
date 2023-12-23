module AdventOfCode.Solutions._2023.Day23

open AdventOfCode.Lib.Solver
open System

// let dx = [|-1; 0; 1; 0|]
// let dy = [|0; -1; 0; 1|]
// let left = 0
// let up = 1
// let right = 2
// let down = 3
let dx = [| 1; 0; -1; 0 |]
let dy = [| 0; 1; 0; -1 |]
let right = 0
let down = 1
let left = 2
let up = 3

[<AocSolver(2023, 23, Level = 1)>]
let solve1 (input: string list) =
    let maze = input |> array2D

    let startX = maze[0, *] |> Array.findIndex (fun c -> c = '.')
    let targetY = (maze |> Array2D.length1) - 1

    let S = Set.empty
    let rec walk (x, y) cost S best =
        if y = targetY then max cost best
        else
            let neighbours =
                [0..3]
                |> List.map (fun i -> (x + dx[i], y + dy[i]), i)
                |> List.filter (fun ((x, y), _dir) -> x >= 0 && x < Array2D.length2 maze && y >= 0 && y < Array2D.length1 maze)
                |> List.reject (fun ((x, y), _dir) -> maze[y, x] = '#')
                |> List.filter (fun ((x, y), dir) ->
                    match maze[y, x] with
                    | '>' -> dir = right
                    | 'v' -> dir = down
                    | '<' -> dir = left
                    | '^' -> dir = up
                    | _ -> true)
                |> List.reject (fun ((x, y), _dir) -> Set.contains (x, y) S)
            List.fold (fun best ((x, y), _dir) ->
                let S = Set.add (x, y) S
                let best = walk (x, y) (cost + 1) S best
                best) best neighbours

    walk (startX, 0) 0 (Set.add (startX, 0) S) 0


[<AocSolver(2023, 23, Level = 2)>]
let solve2 (input: string list) =
    let maze = input |> array2D

    let startX = maze[0, *] |> Array.findIndex (fun c -> c = '.')
    let targetY = (maze |> Array2D.length1) - 1

    // find all intersections
    let intersections =
        Array2D.foldi (fun state y x value ->
            match value with
            | '#' -> state
            | _ ->
                let neighbours =
                    [0..3]
                    |> List.map (fun i -> (x + dx[i], y + dy[i]), i)
                    |> List.filter (fun ((x, y), _dir) -> x >= 0 && x < Array2D.length2 maze && y >= 0 && y < Array2D.length1 maze)
                    |> List.reject (fun ((x, y), _dir) -> maze[y, x] = '#')
                if List.length neighbours > 2 || y = 0 || y = targetY then Set.add (x, y) state
                else state) Set.empty maze

    // Find paths between intersections
    // I'm looking to find a FROM (x, y), TO (x, y), LENGTH.
    // Don't go past an intersection
    let paths =
        Set.toList intersections
        |> List.fold (fun map (x, y) ->
            let rec walk Q S paths =
                if Queue.isEmpty Q then List.rev paths
                else
                    let cur = Queue.front Q
                    let Q = Queue.dequeue Q

                    let (curX, curY), cost = cur
                    if (curX <> x || curY <> y) && Set.contains (curX, curY) intersections then
                        walk Q S (((x, y), (curX, curY), cost) :: paths)
                    else
                        let S = Set.add (curX, curY) S
                        let neighbours =
                            [0..3]
                            |> List.map (fun i -> (curX + dx[i], curY + dy[i]), i)
                            |> List.filter (fun ((x, y), _dir) -> x >= 0 && x < Array2D.length2 maze && y >= 0 && y < Array2D.length1 maze)
                            |> List.reject (fun ((x, y), _dir) -> maze[y, x] = '#')
                            |> List.reject (fun ((x, y), _dir) -> Set.contains (x, y) S)
                        let Q = List.fold (fun Q ((x, y), _dir) -> Queue.enqueue ((x, y), cost + 1) Q) Q neighbours
                        // let S = List.fold (fun S ((x, y), _dir) -> Set.add (x, y) S) S neighbours
                        walk Q S paths
            let P = walk (Queue.singleton ((x, y), 0)) (Set.singleton (x, y)) []
            Map.add (x, y) P map) Map.empty

    let rec search curX curY cost S best =
        if curY = targetY then
            max best cost
        else
            let next = Map.find (curX, curY) paths
            let next = List.reject (fun (_from, (toX, toY), _len) -> Set.contains (toX, toY) S) next
            next
            |> List.fold (fun best (_from, (toX, toY), len) ->
                    let S = Set.add (toX, toY) S
                    max best (search toX toY (cost + len) S  best)) best

    search startX 0 0 (Set.singleton (startX, 0)) 0
