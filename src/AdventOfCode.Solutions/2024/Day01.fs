namespace AdventOfCode.Solutions._2024

open AdventOfCode.Lib.Solver
open System

module Day01 =
    let parse (str: String) =
         match str.Split(" ", StringSplitOptions.RemoveEmptyEntries) with
         | [| f; s |] -> (Int32.parse(f), Int32.parse(s))
         | _ -> failwith("Unexpected")

    [<AocSolver(2024, 1, Level = 1)>]
    let solve1 (input: string list) =
        let parsed =
            input
            |> List.map parse

        let (first, second) = List.unzip parsed

        List.zip (List.sort first) (List.sort second)
        |> List.map (fun (a, b) -> abs(a - b))
        |> List.sum

    [<AocSolver(2024, 1, Level = 2)>]
    let solve2 (input: string list) =
        let parsed =
            input
            |> List.map parse

        let (first, second) = List.unzip parsed

        first
        |> List.map (fun s -> s * List.countWhere (fun n -> n = s) second)
        |> List.sum
