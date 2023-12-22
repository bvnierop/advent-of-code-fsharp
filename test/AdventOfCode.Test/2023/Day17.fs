module AdventOfCode.Test._2023.Day17

open System
open Xunit
open Swensen.Unquote
open AdventOfCode.Solutions._2023.Day17

[<Fact>]
let ``solve part 2`` () =
    let input =
        [ "111111111111"
          "999999999991"
          "999999999991"
          "999999999991"
          "999999999991" ]
    test <@ solve2 input = 71 @>
