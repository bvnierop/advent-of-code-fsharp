module AdventOfCode.Test._2023.Day05

open Xunit
open Swensen.Unquote
open AdventOfCode.Solutions._2023.Day05
open AdventOfCode.Lib

[<Fact>]
let ``translate with ranges`` () =
    let seedRange = Range.create 10L 20L
    let seedToSoilRange1: AlmanacMap.t = { SourceRange = Range.create 15L 19L; DestinationStart = 100L }
    let seedToSoilRange2: AlmanacMap.t = { SourceRange = Range.create 10L 11L; DestinationStart = 200L }

    let result = AlmanacMap.translateWithRanges seedRange [seedToSoilRange1; seedToSoilRange2]
    test <@ result = [ Range.create 11 15; Range.create 19 20; Range.create 100 104; Range.create 200 201; ] @>

[<Fact>]
let ``translate ranges through the almanac`` () =
    let almanac = Almanac.make [] [ AlmanacMap.make [ (50, 98, 2); (52, 50, 48) ]; AlmanacMap.make [ (0, 10, 100) ] ]
    test <@ Almanac.translateRange (Range.create 55L 56L) almanac = [Range.create 47L 48L] @>

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
