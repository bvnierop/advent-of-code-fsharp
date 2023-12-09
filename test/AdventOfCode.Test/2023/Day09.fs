module AdventOfCode.Test._2023.Day09

open System
open Xunit
open Swensen.Unquote
open AdventOfCode.Solutions._2023.Day09

[<Fact>]
let ``find differences`` () =
    differences [ 0; 3; 6; 9; 12 ] =! [ 3; 3; 3; 3 ]

[<Fact>]
let ``find differences until no more differences exist`` () =
    differencesUntilNone [ 0; 3; 6; 9; 12 ]
    =! [ [ 3; 3; 3; 3 ]; [ 0; 0; 0 ]; [ 0; 0 ]; [ 0 ] ]

let extrapolateTestData () : obj array seq = seq {
     yield [| [ 0; 3; 6; 9; 12; 15 ]; 18 |]
     yield [| [ 1; 3; 6; 10; 15; 21 ]; 28 |]
     yield [| [ 10; 13; 16; 21; 30; 45 ]; 68 |]
}

[<Theory>]
[<MemberData(nameof(extrapolateTestData))>]
let ``extrapolate the next number`` numbers expected =
    extrapolateNextNumber numbers =! expected

let testInput = [
    "0 3 6 9 12 15"
    "1 3 6 10 15 21"
    "10 13 16 21 30 45"
]

[<Fact>]
let ``solve part 1`` () =
    solve1 testInput =! 114

[<Fact>]
let ``solve part 2`` () =
    solve2 testInput =! 2
