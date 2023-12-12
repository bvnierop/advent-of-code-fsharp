module AdventOfCode.Test._2023.Day12

open System
open Xunit
open Swensen.Unquote
open AdventOfCode.Solutions._2023.Day12

[<Fact>]
let ``parse a line`` () =
    test <@ parseLine ".#?## 1,2" = { States = [ Operational; Damaged; Unknown; Damaged; Damaged ]
                                      OperationalGroups =  [1; 2] } @>

let processGroupData () : obj array seq = seq {
    yield [| "#.. 1"; Some ". " |]
    yield [| "##.. 2"; Some ". " |]
    yield [| "##?.# 3,1"; Some "# 1" |]
    yield [| "###.# 2,1"; None |]
    yield [| "##?.# 2,1"; Some ".# 1" |]
    yield [| "### 4"; None |]
    yield [| "# "; None |] // unexpected group
    yield [| "## 2"; Some " " |]
    yield [| "##. 2"; Some " " |]
}

[<Theory>]
[<MemberData(nameof(processGroupData))>]
let ``process group`` input expected =
    let record = input |> parseLine
    let expectedRecord =
        match expected with
        | Some str -> str |> parseLine |> Some
        | None -> None

    test <@ tryProcessGroup record = expectedRecord @>

[<Fact>]
let ``find number of possibilities`` () =
    let record = "..??..??.. 1,1" |> parseLine
    test <@ numberOfPossibilities record = 4 @>

[<Fact>]
let ``unfolding rows`` () =
    let record = ".#?## 1,2" |> parseLine
    let expected = ".#?##?.#?## 1,2,1,2" |> parseLine
    test <@ SpringRow.unfold 2 record = expected @>
