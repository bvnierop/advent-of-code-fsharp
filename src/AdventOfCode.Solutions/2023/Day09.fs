namespace AdventOfCode.Solutions._2023

open AdventOfCode.Lib.Solver
open FParsec

module Day09 =
    let pLine = sepEndBy pint32 spaces
    let parseLine = parseOrDie pLine

    let differences numbers =
        List.pairwise numbers |> List.map (fun (a, b) -> b - a)

    let differencesUntilNone numbers =
        List.unfold
            (fun numbers ->
                match differences numbers with
                | [] -> None
                | differences -> Some(differences, differences))
            numbers

    let extrapolateNextNumber numbers =
        differencesUntilNone numbers |> List.sumBy List.last |> (+) (List.last numbers)

    [<AocSolver(2023, 9, Level = 1)>]
    let solve1 (input: string list) =
        input
        |> List.map parseLine
        |> List.sumBy extrapolateNextNumber

    [<AocSolver(2023, 9, Level = 2)>]
    let solve2 (input: string list) =
        input
        |> List.map (parseLine >> List.rev)
        |> List.sumBy extrapolateNextNumber
