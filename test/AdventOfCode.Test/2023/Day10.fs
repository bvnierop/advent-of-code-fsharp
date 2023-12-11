module AdventOfCode.Test._2023.Day10

open System
open Xunit
open Swensen.Unquote
open AdventOfCode.Solutions._2023.Day10

let testInput = [
    "..F7."
    ".FJ|."
    "SJ.L7"
    "|F--J"
    "LJ..."
]

[<Fact>]
let ``solve part 1`` () =
    solve1 testInput =! 8
