module AdventOfCode.Test._2023.Day13

open System
open Xunit
open Swensen.Unquote
open AdventOfCode.Solutions._2023.Day13

[<Fact>]
let ``parse input`` () =
    let input = """##
#.

#.
.."""

    let expected = [
        [ "##" ; "#." ] |> array2D
        [ "#." ; ".." ] |> array2D
    ]

    test <@ parse input =  expected  @>

let mirrorRowExamples () : obj array seq = seq {
    yield [| [ "##"; "##" ]; 0; true |]
    yield [| [ ".."; "##"; "##" ]; 0; false |]
    yield [| [ ".."; "##" ]; 1; false |]
    yield [| [ ".."; "##"; "##"; "#." ]; 1; false |]

    yield [| [ ".."; "##"; "##" ]; 1; true |]
    yield [| [ "##"; "##"; ".." ]; 0; true |]
}

[<Theory>]
[<MemberData(nameof(mirrorRowExamples))>]
let ``check if a row is a mirror`` (input: string list) (row: int) (expected: bool) =
    let pattern = input |> array2D
    test <@ Pattern.isMirrorSlice pattern Pattern.rowSlicer row = expected @>

[<Fact>]
let ``find index of mirroring row slice`` () =
    let pattern = [ ".."; "##"; "##"; "#." ; "#."; "##" ] |> array2D

    test <@ Pattern.findMirrorSlice Pattern.rowSlicer pattern = Some 3 @>

[<Fact>]
let ``find index of mirroring column slice`` () =
    let pattern =
        [
            "#.##..##."
            "..#.##.#."
            "##......#"
            "##......#"
            "..#.##.#."
            "..##..##."
            "#.#.##.#."
        ]
        |> array2D

    test <@ Pattern.findMirrorSlice Pattern.columnSlicer pattern = Some 4 @>

[<Fact>]
let ``fail to find index of mirroring slice when there is none`` () =
    let pattern = [ ".."; "##"; "##"; "#." ; "##"; ".#" ] |> array2D

    test <@ Pattern.findMirrorSlice Pattern.rowSlicer pattern = None @>

[<Fact>]
let ``count differences`` () =
    let a = [| '#'; '#'; '#'; '#' |]
    let b = [| '#'; '.'; '.'; '#' |]

    test <@ Array.countDifferences a b = 2 @>

[<Fact>]
let ``find first difference`` () =
    let a = [| '#'; '#'; '#'; '#' |]
    let b = [| '#'; '.'; '.'; '#' |]

    test <@ Array.findFirstDifference a b = Some 1 @>
    test <@ Array.findFirstDifference a a = None @>
