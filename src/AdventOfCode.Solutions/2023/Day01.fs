namespace AdventOfCode.Solutions._2023

open System.Text.RegularExpressions
open AdventOfCode.Lib.Solver
open System

module Day01 =

    let extractCalibrationValue str =
       let digits =
           str
           |> String.toArray
           |> Array.filter Char.isDigit
       [| digits[0]; Array.last digits |]
       |> String |> Int32.Parse

    let lookup = Map [
        ("1",'1'); ("2",'2'); ("3",'3'); ("4",'4'); ("5",'5'); ("6",'6'); ("7",'7'); ("8",'8');
        ("9",'9'); ("0",'0'); ("one",'1'); ("two",'2'); ("three",'3'); ("four",'4'); ("five",'5');
        ("six",'6'); ("seven",'7'); ("eight",'8'); ("nine",'9')
    ]

    let extractWrittenCalibrationValue str =
        let re = "(\d|one|two|three|four|five|six|seven|eight|nine)";
        [| Regex.Match(str, re); Regex.Match(str, re, RegexOptions.RightToLeft) |]
        |> Array.map (fun m -> m.Value)
        |> Array.map (fun s -> lookup[s])
        |> String
        |> Int32.Parse

    [<AocSolver(2023, 1, Level = 1)>]
    let solve1 (input: string list) =
        List.sumBy extractCalibrationValue input

    [<AocSolver(2023, 1, Level = 2)>]
    let solve2 (input: string list) =
        List.sumBy extractWrittenCalibrationValue input
