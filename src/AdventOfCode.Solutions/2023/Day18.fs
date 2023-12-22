module AdventOfCode.Solutions._2023.Day18

open System
open System.Text
open AdventOfCode.Lib.Solver
open FParsec

(*
R 6 (#70c710)
D 5 (#0dc571)
L 2 (#5713f0)
D 2 (#d2c081)
R 2 (#59c680)
D 2 (#411b91)
L 5 (#8ceee2)
U 2 (#caa173)
L 1 (#1b58a2)
U 2 (#caa171)
R 2 (#7807d2)
U 3 (#a77fa3)
L 2 (#015232)
U 2 (#7a21e3)
*)

let dx = [| 1; 0; -1; 0 |]
let dy = [| 0; 1; 0; -1 |]
type Direction = Right = 0 | Down = 1 | Left = 2 | Up = 3
module Direction =
    let pDirection = (pchar 'R' >>% Direction.Right) <|> (pchar 'D' >>% Direction.Down) <|> (pchar 'L' >>% Direction.Left) <|> (pchar 'U' >>% Direction.Up)

type Step = Direction * int
module Step =
    let pStep = Direction.pDirection .>> spaces1 .>>. pint32
// R 6 (#70c710)
    let pStep2 =
        skipCharsTillString "(#" true 100 >>.
        (anyString 5 |>> (fun s -> Convert.ToInt32(s, 16))) .>>. (pint32 |>> enum<Direction>) ||>> (fun steps dir -> dir, steps)

    let parse = parseOrDie pStep
    let expand (dir, steps) = [1..steps] |> List.map (fun _i -> (dx[int dir], dy[int dir]))
    let parse2 = parseOrDie pStep2

let show (map: bool[,]) =
    let sb = StringBuilder()
    for y in 0..Array2D.length1 map - 1 do
        for x in 0..Array2D.length2 map - 1 do
            sb.Append (if map[y, x] then '#' else '.') |> ignore
        sb.AppendLine() |> ignore
    sb.ToString()

[<AocSolver(2023, 18, Level = 1)>]
let solve1 (input: string list) =
    let (_, _), points =
        input
        |> List.map Step.parse
        |> List.collect Step.expand
        |> List.fold (fun ((x, y), map) (dx, dy) ->
            let nx = x + dx
            let ny = y + dy
            (nx, ny), Set.add (nx, ny) map) ((0, 0), Set.add (0, 0) Set.empty)

    let pointList = points |> Set.toList
    let minX = List.minBy fst pointList |> fst
    let maxX = List.maxBy fst pointList |> fst
    let minY = List.minBy snd pointList |> snd
    let maxY = List.maxBy snd pointList |> snd

    // Add a border around the map for easier flood fill
    let width = maxX - minX + 3
    let height = maxY - minY + 3

    let map = Array2D.init height width (fun y x -> points |> Set.contains (x + minX - 1, y + minY - 1))

    let Q = Queue.empty
    let S = Set.empty
    let rec flood Q S (map: bool[,]) =
        if Queue.isEmpty Q then (Array2D.length1 map * Array2D.length2 map) - Set.length S
        else
            let cur = Queue.front Q
            let x, y = cur
            let Q = Queue.dequeue Q
            let neighbours = [(x + 1, y); (x - 1, y); (x, y + 1); (x, y - 1)]
            let neighbours =
                neighbours
                |> List.filter (fun (x, y) -> x >= 0 && x < Array2D.length2 map && y >= 0 && y < Array2D.length1 map) // outside array
                |> List.reject (fun (x, y) -> Set.contains (x, y) S)  // already visited
                |> List.reject (fun (x, y) -> map[y, x]) // dug out
            let Q = List.fold (fun Q (x, y) -> Queue.enqueue (x, y) Q) Q neighbours
            let S = List.fold (fun S (x, y) -> Set.add (x, y) S) S neighbours
            flood Q S map
    flood (Queue.enqueue (0, 0) Q) (Set.add (0, 0) S) map


[<AocSolver(2023, 18, Level = 2)>]
let solve2 (input: string list) =
    let instructions = input |> List.map Step.parse2

    let points =
        instructions
        |> List.fold (fun ((x, y), points) (dir, steps) ->
            let nx = (x + int64 dx[int dir] * int64 steps) |> int64
            let ny = (y + int64 dy[int dir] * int64 steps) |> int64
            (nx, ny), ((nx, ny) :: points)) ((0L, 0L), [])
        |> snd |> List.rev

    let shoelaceFormula (points: (int64 * int64) list) : int64 =
        let sum =
            List.pairwise points
            |> List.sumBy (fun ((x1, y1), (x2, y2)) -> x1 * y2 - x2 * y1)
        Math.Abs(sum / 2L)

    let perimeter = instructions |> List.sumBy snd |> int64

    shoelaceFormula points + (perimeter / 2L) + 1L
