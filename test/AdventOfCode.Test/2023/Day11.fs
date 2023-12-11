module AdventOfCode.Test._2023.Day11

open System
open Xunit
open Swensen.Unquote
open AdventOfCode.Solutions._2023.Day11

let testInput =
     [
     "...#......"
     ".......#.."
     "#........."
     ".........."
     "......#..."
     ".#........"
     ".........#"
     ".........."
     ".......#.."
     "#...#....."
     ]

[<Fact>]
let ``sum all distances`` () =
    test <@ solve1 testInput = 374 @>

[<Fact>]
let ``calculate costs for a cell`` () =
    let input = [
        "#.."
        "..."
        "..#"
    ]

    let costs = [
        [ 1; 2; 1 ]
        [ 2; 4; 2 ]
        [ 1; 2; 1 ]
    ]

    test <@ input |> parse |> calculateCosts 2 = (costs |> array2D |> Array2D.map int64) @>

[<Fact>]
let ``calculate costs for a cell when it's expansive`` () =
    let input = [
        "#.."
        "..."
        "..#"
    ]

    let costs = [
        [ 1; 20; 1 ]
        [ 20; 400; 20 ]
        [ 1; 20; 1 ]
    ]

    test <@ input |> parse |> calculateCosts 20 = (costs |> array2D |> Array2D.map int64) @>

[<Fact>]
let ``calculate the cost for a path`` () =
    let costs =
        [[ 1; 2; 1 ]
         [ 2; 4; 2 ]
         [ 1; 2; 1 ]]
        |> array2D |> Array2D.map int64
    test <@ calculatePathCost (0, 0) (2, 2) costs = 6L @>

[<Fact>]
let ``sum all distances when it's expansive`` () =
    test <@ solve 10L testInput = 1030L @>
    test <@ solve 100L testInput = 8410L @>
