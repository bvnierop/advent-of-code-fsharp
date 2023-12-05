module RangeTests

open Xunit
open Swensen.Unquote
open AdventOfCode.Lib

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
        let range = Range.createWithLength 98 2
        test <@ Range.contains 97 range = false @>
        test <@ Range.contains 98 range = true @>
        test <@ Range.contains 99 range = true @>
        test <@ Range.contains 100 range = false @>

    [<Fact>]
    let ``range supports int64`` () =
        let range = Range.create 0L 10L
        test <@ Range.length range = 10L @>

    [<Fact>]
    let ``range supports int32`` () =
        let range = Range.create 0 10
        test <@ Range.length range = 10 @>
