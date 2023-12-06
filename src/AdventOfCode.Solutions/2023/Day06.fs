namespace AdventOfCode.Solutions._2023

open AdventOfCode.Lib.Solver
open FParsec

module Day06 =
    type Time = Time of int
    type Distance = Distance of int64

    let discriminant (a: double) (b: double) (c: double) = sqrt ((b * b) - (4.0 * a * c))

    let firstWinCondition (Time t) (Distance d) =
        let b = double t
        let c = double d
        int <| floor ((b - discriminant 1 b c) / 2.0 + 1.0)

    let lastWinCondition (Time t) (Distance d) =
        let b = double t
        let c = double d
        int <| ceil ((b + discriminant 1 b c) / 2.0 - 1.0)

    let waysToWin (Time t) (Distance d) =
        lastWinCondition (Time t) (Distance d) - firstWinCondition (Time t) (Distance d) + 1

    let pLine = manyCharsTill anyChar (pchar ':') >>. spaces >>. sepEndBy pint32 spaces

    let parseInput lines =
        match List.map (parseOrDie pLine) lines with
        | [ times; distances ] -> List.map2 (fun t d -> (Time t), (Distance (int64 d))) times distances
        | _ -> failwith "Invalid input"

    [<AocSolver(2023, 6, Level = 1)>]
    let solve1 (input: string list) =
        parseInput input
        |> List.map (fun (t, d) -> waysToWin t d)
        |> List.fold (*) 1

    [<AocSolver(2023, 6, Level = 2)>]
    let solve2 (input: string list) =
        parseInput input
        |> List.fold (fun (Time totalTime, Distance totalDistance) (Time t, Distance d) ->
            $"{totalTime}{t}" |> Int32.parse |> Time,
            $"{totalDistance}{d}" |> Int64.parse |> Distance) (Time 0, Distance 0)
        ||> waysToWin
