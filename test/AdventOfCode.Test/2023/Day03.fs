module AdventOfCode.Test._2023.Day03

open System
open System.IO
open Xunit
open Swensen.Unquote
open AdventOfCode.Solutions._2023.Day03

[<Fact>]
let ``parse a line`` () =
    let line = "467..114..$"
    let expected = [| (Digit 4); (Digit 6); (Digit 7); Nothing; Nothing; (Digit 1); (Digit 1); (Digit 4); Nothing; Nothing; OtherPart |]
    test <@ parseLine line = expected @>

[<Fact>]
let ``convert digits to numbers`` () =
    let line = "467..114..*"
    let firstNumber = Number (467, 0)
    let secondNumber = Number (114, 1)
    let expected = [| firstNumber; firstNumber; firstNumber; Nothing; Nothing; secondNumber; secondNumber; secondNumber; Nothing; Nothing; PotentialGear |]
    test <@ convertDigits 0 (parseLine line) = (expected, 2) @>

[<Fact>]
let ``convert multiple lines`` () =
    let lines = [
        "467..114..$"
        ".........42"
    ]

    let firstNumber = Number (467, 0)
    let secondNumber = Number (114, 1)
    let thirdNumber = Number (42, 2)

    let expected = [|
        [| firstNumber; firstNumber; firstNumber; Nothing; Nothing; secondNumber; secondNumber; secondNumber; Nothing; Nothing; OtherPart |]
        [|Nothing; Nothing; Nothing; Nothing; Nothing; Nothing; Nothing; Nothing; Nothing;  thirdNumber; thirdNumber |]
    |]
    test <@ parse lines = expected @>

[<Fact>]
let ``Part 1 from test data`` () =
    let inputFile = "input/2023/03-test.in"
    let expectedFile = "input/2023/03-test-1.out"
    let input = File.ReadAllText(inputFile).Replace("\r\n", "\n").TrimEnd().Split("\n") |> Array.toList
    let expected = File.ReadAllText(expectedFile).Replace("\r\n", "\n").TrimEnd()
    test <@ $"{solve1 input}" = expected @>

[<Fact>]
let ``Part 2 from test data`` () =
    let inputFile = "input/2023/03-test.in"
    let expectedFile = "input/2023/03-test-2.out"
    let input = File.ReadAllText(inputFile).Replace("\r\n", "\n").TrimEnd().Split("\n") |> Array.toList
    let expected = File.ReadAllText(expectedFile).Replace("\r\n", "\n").TrimEnd()
    test <@ $"{solve2 input}" = expected @>
