module AdventOfCode.Test._2023.Day06

open Xunit
open Swensen.Unquote
open AdventOfCode.Solutions._2023.Day06

[<Fact>]
let ``count number of ways to win`` () =
    let t = (Time 7)
    let d = (Distance 9)
    test <@ waysToWin t d = 4 @>

[<Fact>]
let ``parse input`` () =
    let lines = [
        "Time:      7  15   30"
        "Distance:  9  40  200"
    ]
    test <@ parseInput lines = [(Time 7, Distance 9); (Time 15, Distance 40); (Time 30, Distance 200)] @>

[<Fact>]
let ``solve problem 1`` () =
    let lines = [
        "Time:      7  15   30"
        "Distance:  9  40  200"
    ]
    test <@ solve1 lines = 288 @>

[<Fact>]
let ``solve problem 2`` () =
    let lines = [
        "Time:      7  15   30"
        "Distance:  9  40  200"
    ]
    test <@ solve2 lines = 71503 @>
