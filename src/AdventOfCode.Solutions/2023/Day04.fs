namespace AdventOfCode.Solutions._2023

open AdventOfCode.Lib.Solver
open System
open FParsec

module Day04 =
    type Card = (int list * int list)

    let ws = spaces

    let pLine = pstring "Card" >>. ws >>. many1Satisfy isDigit .>> ws .>> pchar ':' .>> ws >>.
                sepEndBy pint32 ws .>> pchar '|' .>> ws .>>. sepEndBy pint32 ws
    let parse line =
        parseOrDie pLine line

    let score (winning, have) =
        let winningNumberCount = Set.intersect (Set.ofList have) (Set.ofList winning)
                                 |> Set.length
        max (1 <<< (winningNumberCount - 1)) 0

    let winningsOf cardIndex (cards: Card array) =
        let winning, have = cards[cardIndex]
        let winningNumberCount = Set.intersect (Set.ofList have) (Set.ofList winning)
                                 |> Set.length
        [ for i = 1 to winningNumberCount do yield cardIndex + i ]

    let totalWinningsOf =
        let mutable memo = Map.empty
        let rec totalWinningsOf (cards: Card array) cardIndex =
            if Map.containsKey cardIndex memo then
                Map.find cardIndex memo
            else
                let winnings = winningsOf cardIndex cards
                let totalWinnings = List.sumBy (totalWinningsOf cards) winnings
                let total = List.length winnings + totalWinnings
                memo <- Map.add cardIndex total memo
                total
        totalWinningsOf


    let scratch (cards: Card array) =
        [ 0..Array.length cards - 1 ]
        |> List.sumBy (totalWinningsOf cards)
        |> ((+) (Array.length cards))

    [<AocSolver(2023, 4, Level = 1)>]
    let solve1 (input: string list) =
        input
        |> List.sumBy (parse >> score)

    [<AocSolver(2023, 4, Level = 2)>]
    let solve2 (input: string list) =
        input |> List.map parse |> Array.ofList |> scratch
