module AdventOfCode.Test._2023.Day01

open System
open Xunit
open Swensen.Unquote
open AdventOfCode.Solutions._2023.Day01

[<Fact>]
let ``take the first and the last digit of a string`` () =
    test <@ extractCalibrationValue "a1b2c3" = 13 @>

[<Fact>]
let ``Day01, part 1 sums a list of calibtration values`` () =
    test <@ solve1 ["a1b"; "2abv2"] = 33 @>

[<Fact>]
let ``also take spelled numbers into account`` () =
    test <@ extractWrittenCalibrationValue "7pqrstsixteen" = 76 @>

[<Fact>]
let ``Day01, part 2 sums a list of calibtration values with written digits`` () =
    test <@ solve2 ["aonebonec"; "2abv2"] = 33 @>
