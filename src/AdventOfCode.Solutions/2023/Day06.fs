namespace AdventOfCode.Solutions._2023

open AdventOfCode.Lib.Solver
open FParsec

module Day06 =
    type Time = Time of int
    type Distance = Distance of int64

    let waysToWin (Time t) (Distance d) =
        [ for i in 0 .. t do yield int64 i * ((int64 t) - (int64 i)) ]
        |> List.filter (fun x -> x > d)
        |> List.length

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
