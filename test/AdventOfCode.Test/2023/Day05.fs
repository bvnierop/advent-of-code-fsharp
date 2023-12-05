module AdventOfCode.Test._2023.Day05

open Xunit
open Swensen.Unquote
open AdventOfCode.Solutions._2023.Day05

module RangeTests =
    [<Theory>]
    [<InlineData(0, 1, 0, 1, false)>]
    [<InlineData(0, 3, 1, 2, false)>]
    [<InlineData(0, 1, 1, 2, true)>]
    let ``ranges can be disjoint`` start1 end1 start2 end2 disjoint =
        test <@ Range.disjoint (Range.create start1 end1) (Range.create start2 end2) = disjoint @>
        test <@ Range.disjoint (Range.create start2 end2) (Range.create start1 end1) = disjoint @>

    [<Theory>]
    [<InlineData(0, 10, 5, 20, 5, 10)>]
    [<InlineData(0, 10, 10, 20, 10, 10)>]
    [<InlineData(0, 10, 20, 30, 20, 10)>]
    let ``ranges can have intersections`` start1 end1 start2 end2 interStart interEnd =
        let range1 = Range.create start1 end1
        let range2 = Range.create start2 end2
        let expected = Range.create interStart interEnd
        test <@ Range.intersection range1 range2 = expected @>
        test <@ Range.intersection range2 range1 = expected @>

    [<Fact>]
    let ``a range can be shifted`` () =
        let range = Range.create 0L 10L
        test <@ Range.shift 5L range = Range.create 5 15 @>
        test <@ Range.shift -5L range = Range.create -5 5 @>

    [<Fact>]
    let ``a range can be split`` () =
        let range = Range.create 0 10
        test <@ Range.split 4 range = (Range.create 0 4, Range.create 4 10) @>
        test <@ Range.split 12 range = (Range.create 0 10, Range.create 10 10) @>

    [<Fact>]
    let ``a range can have differences`` () =
        let range1 = Range.create 0L 20L
        let range2 = Range.create 5L 15L
        test <@ Range.difference range1 range2 = [Range.create 0 5; Range.create 15 20] @>
        test <@ Range.difference range2 range1 = [] @>

    [<Fact>]
    let ``a range can have a single difference`` () =
        let range1 = Range.create 0L 20L
        let range2 = Range.create 0L 30L
        let range3 = Range.create -10L 20L
        test <@ Range.difference range1 range2 = [] @>
        test <@ Range.difference range1 range3 = [] @>
        test <@ Range.difference range2 range1 = [Range.create 20 30] @>
        test <@ Range.difference range3 range1 = [Range.create -10 0] @>

    [<Fact>]
    let ``if ranges don't overlap, range1 is the entire difference`` () =
        let range1 = Range.create 0L 10L
        let range2 = Range.create 20L 30L
        test <@ Range.difference range1 range2 = [Range.create 0 10 ] @>
        test <@ Range.difference range2 range1 = [Range.create 20 30 ] @>


    [<Theory>]
    [<InlineData(0, 10, 10)>]
    [<InlineData(10, 10, 0)>]
    [<InlineData(15, 10, 0)>]
    let ``a range has a length`` start1 end1 length =
        let range = Range.create start1 end1
        test <@ Range.length range = length @>

    [<Fact>]
    let ``range contains`` () =
        let range = Range.makeWithLength 98 2
        test <@ Range.contains 97L range = false @>
        test <@ Range.contains 98L range = true @>
        test <@ Range.contains 99L range = true @>
        test <@ Range.contains 100L range = false @>

    [<Fact>]
    let ``range supports int64`` () =
        let range = Range.create 0L 10L
        test <@ Range.length range = 10L @>

    [<Fact>]
    let ``range supports int32`` () =
        let range = Range.create 0 10
        test <@ Range.length range = 10 @>

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
