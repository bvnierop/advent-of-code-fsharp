module AdventOfCode.Solutions._2023.Day24

open AdventOfCode.Lib.Solver
open FParsec
let pDecimal = pint64 |>> decimal
let pSep = pchar ',' .>> spaces
let pDecs = pipe3 (pDecimal .>> pSep) (pDecimal.>> pSep) pDecimal (fun a b c -> a, b, c)
let pLines = pDecs .>> spaces .>> pchar '@' .>> spaces .>>. pDecs
let parseLine = parseOrDie pLines

let intersection ((x1, y1, _z1), (vx1, vy1, _vz1)) ((x2, y2, _z2), (vx2, vy2, _vz2)) =
    let a = vy1 / vx1 // slope
    let c = y1 - a * x1 // y-intersept

    let b = vy2 / vx2
    let d = y2 - b * x2

    if a = b || c = d then None // a = b > parallel /  c = d > same line
    else
        let ix = (d - c) / (a - b)
        let iy = a * ix + c
        if (ix > x1) = (vx1 > 0m) && (ix > x2) = (vx2 > 0m) then Some (ix, iy)
        else None

let insideArea ((ax1, ay1), (ax2, ay2)) (x, y) =
    x >= ax1 && x <= ax2 && y >= ay1 && y <= ay2

[<AocSolver(2023, 24, Level = 1)>]
let solve1 (input: string list) =
    let lines = input |> List.map parseLine

    let inputSize = List.length lines
    let targetMin, targetMax = if inputSize <= 100 then (7m, 27m) else (200000000000000m, 400000000000000m)

    List.pairs lines
    |> Seq.toList
    |> List.map (fun (l1, l2) -> intersection l1 l2)
    |> List.choose id
    |> List.filter (insideArea ((targetMin, targetMin), (targetMax, targetMax)))
    |> List.length

[<AocSolver(2023, 24, Level = 2)>]
let solve2 (input: string list) =
    2
