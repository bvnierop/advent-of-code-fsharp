namespace AdventOfCode.Solutions._2023

open AdventOfCode.Lib.Solver
open System
open FParsec

module Day02 =
    type Game = {
        Red: int
        Green: int
        Blue: int
    }

    let pTurn = sepBy (pint32 .>>. (spaces >>. (pstring "green" <|> pstring "red" <|> pstring "blue"))) (pstring ", ")
    let pGame = pstring "Game " >>. pint32
    let pLine = pGame .>>. pstring ": " >>. sepBy pTurn (pstring "; ")

    let parseLine line =
        let turns = parseOrDie pLine line
        turns
        |> List.fold (fun game turn ->
               List.fold (fun game cube ->
                   match cube with
                   | x, "red" -> { game with Game.Red = max game.Red x }
                   | x, "green" -> { game with Game.Green = max game.Green x }
                   | x, "blue" -> { game with Game.Blue = max game.Blue x }
                   | _ -> game) game turn
            ) { Red = 0; Green = 0; Blue = 0 }

    [<AocSolver(2023, 2, Level = 1)>]
    let solve1 (input: string list) =
        input
        |> List.map parseLine
        |> List.indexed
        |> List.filter (fun (_, game) -> game.Red <= 12 && game.Green <= 13 && game.Blue <= 14)
        |> List.sumBy (fst >> (+) 1)

    [<AocSolver(2023, 2, Level = 2)>]
    let solve2 (input: string list) =
        input
        |> List.map parseLine
        |> List.sumBy (fun game -> game.Red * game.Green * game.Blue)
