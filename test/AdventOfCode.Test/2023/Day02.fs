module AdventOfCode.Test._2023.Day02

open System
open Xunit
open Swensen.Unquote
open AdventOfCode.Solutions._2023.Day02

[<Fact>]
let ``parse input`` () =
    let inputLine = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
    let expected = {
        Blue = 6
        Red = 4
        Green = 2
    }
    test <@ parseLine inputLine = expected @>

[<Fact>]
let ``Part 1 sums valid game ids`` () =
    let input = [
        "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green";
        "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue";
        "Game 3: 8 green, 6 blue, 13 red; 5 blue, 4 red, 13 green; 5 green, 1 red";
        "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red";
        "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
        "Game 6: 6 red, 1 blue, 14 green; 2 blue, 1 red, 2 green"
    ]
    test <@ solve1 input = 8 @>

[<Fact>]
let ``Part 2 sums the power`` () =
    let input = [
        "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green";
        "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue";
        "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red";
        "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red";
        "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
    ]
    test <@ solve2 input = 2286 @>
