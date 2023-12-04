module AdventOfCode.Test._2023.Day04

open System
open Xunit
open Swensen.Unquote
open AdventOfCode.Solutions._2023.Day04

[<Fact>]
let ``parse an input line`` () =
    test
        <@
            parse "Card  3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1" = ([ 1; 21; 53; 59; 44 ],
                                                                        [ 69; 82; 63; 72; 16; 21; 14; 1 ])
        @>

[<Theory>]
[<InlineData("Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1", 2)>]
[<InlineData("Card 42:  1 2 3 4 5 | 1", 1)>]
[<InlineData("Card 43:  1 2 3 4 5 | 1 2 3", 4)>]
[<InlineData("Card 44:  1 2 3 4 5 | 6", 0)>]
let ``calculate score`` input expected =
    let winning, have = parse input
    test <@ score (winning, have) = expected @>

let testData =
    [ "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
      "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19"
      "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1"
      "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83"
      "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36"
      "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11" ]

[<Fact>]
let ``part 1`` () =
    test <@ solve1 testData = 13 @>

[<Fact>]
let ``win copies`` () =
    let cards = List.map parse testData |> Array.ofList
    test <@ winningsOf 0 cards = [1;2;3;4] @>

[<Fact>]
let ``part 2`` () =
    test <@ solve2 testData = 30 @>
