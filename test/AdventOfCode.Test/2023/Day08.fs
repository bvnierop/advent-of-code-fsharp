module AdventOfCode.Test._2023.Day08

open AdventOfCode.Lib
open AdventOfCode.Lib.Math
open Xunit
open Swensen.Unquote
open AdventOfCode.Solutions._2023.Day08

[<Fact>]
let ``parse a line`` () =
    let line = "AAA = (BBB, CCC)"
    parseLine line =! ("AAA", { Left = "BBB"; Right = "CCC" })

let testInput =
    """LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)
"""

[<Fact>]
let ``parse input`` () =
    let testInput =
        """RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
"""
    parseInput testInput
    =! { Directions = CircularCollection.init [ Right; Left ]
         Nodes =
           Map
               [ "AAA", { Left = "BBB"; Right = "CCC" }
                 "BBB", { Left = "DDD"; Right = "EEE" } ] }

[<Fact>]
let ``solve part 1`` () =
    solve1 testInput =! 6

let testInput2 = """LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)"""

[<Fact>]
let ``solve part 2`` () =
    solve2 testInput2 =! 6

[<Fact>]
let ``lists can be equal`` = [1;2;3] =! [1;2;3]
