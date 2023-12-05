module AdventOfCode.Test._2023.Day05

open Xunit
open Swensen.Unquote
open AdventOfCode.Solutions._2023.Day05

[<Fact>]
let ``in range`` () =
    let range = Range.makeWithLength 98 2
    test <@ Range.contains 97L range = false @>
    test <@ Range.contains 98L range = true @>
    test <@ Range.contains 99L range = true @>
    test <@ Range.contains 100L range = false @>

[<Fact>]
let ``translate a value`` () =
    let map = AlmanacMap.make [ (50, 98, 2); (52, 50, 48) ]
    test <@ AlmanacMap.translate 55 map = 57 @>
    test <@ AlmanacMap.translate 98 map = 50 @>
    test <@ AlmanacMap.translate 14 map = 14 @>

[<Fact>]
let ``translate through the almanac`` () =
    let almanac = Almanac.make [] [ AlmanacMap.make [ (50, 98, 2); (52, 50, 48) ]; AlmanacMap.make [ (0, 10, 100) ] ]
    test <@ Almanac.translate 55 almanac = 47L @>

[<Fact>]
let ``parse almanac`` () =
    let str = @"seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15"

    test <@ Almanac.parse str = Almanac.make [79; 14; 55; 13] [ AlmanacMap.make [ (50, 98, 2); (52, 50, 48) ]; AlmanacMap.make [ (0, 15, 37); (37, 52, 2); (39, 0, 15) ] ] @>
