module AdventOfCode.Test._2023.Day22

open System
open Xunit
open Swensen.Unquote
open AdventOfCode.Solutions._2023.Day22

[<Fact>]
let ``parse a block`` () =
    test <@ parse "1,2,3~4,5,6" = ((1, 2, 3), (4, 5, 6)) @>


[<Theory>]
[<InlineData("2,2,2~2,2,2", "3,3,3~4,4,3", false)>]
[<InlineData("2,2,2~2,2,2", "2,2,2~4,4,3", true)>]
[<InlineData("2,2,2~4,4,3", "2,2,2~2,2,2", true)>]
[<InlineData("2,2,2~2,2,2", "2,2,2~2,2,2", true)>]
[<InlineData("2,2,2~2,2,2", "3,1,7~2,1,2", false)>]
[<InlineData("0,1,6~2,1,6", "1,1,8~1,1,9", true)>]
[<InlineData("1,1,8~1,1,9", "0,1,6~2,1,6", true)>]
let ``blocks overlap on x/y coord`` b1 b2 expected =
    let block1 = parse  b1
    let block2 = parse b2

    test <@ overlapXY block1 block2 = expected @>

[<Fact>]
let ``move a block`` () =
    let block = parse "2,2,2~2,2,2"
    test <@ moveTo (0, 0, 0) block = ((0, 0, 0), (0, 0, 0)) @>


[<Theory>]
[<InlineData("0,1,3~2,1,3", "1,1,4~1,1,5", true)>]
let ``supported by`` b1 b2 expected =
    let block1 = parse b1
    let block2 = parse b2

    test <@ supportedBy block1 block2 = expected @>

[<Theory>]
[<InlineData("1,1,4~1,1,5", "0,1,3~2,1,3", true)>]
let supports b1 b2 expected =
    let block1 = parse b1
    let block2 = parse b2

    test <@ supports block1 block2 = expected @>
