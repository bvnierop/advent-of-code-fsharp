namespace AdventOfCode.Solutions._2023

open AdventOfCode.Lib.Solver

module Day11 =
    type Cell =
        | Galaxy
        | EmptySpace

    module Cell =
        let ofChar =
            function
            | '.' -> EmptySpace
            | '#' -> Galaxy
            | _ -> failwith "Invalid cell"

        let toChar =
            function
            | EmptySpace -> '.'
            | Galaxy -> '#'

    let parse input = input |> array2D |> Array2D.map Cell.ofChar

    let isEmptyRow (image: Cell[,]) row =
        image[row,*]
        |> Array.forall ((=) EmptySpace)

    let isEmptyColumn (image: Cell[,]) col =
        image[*,col]
        |> Array.forall ((=) EmptySpace)

    let calculateCosts multiplier image =
        image
        |> Array2D.mapi (fun row col _cell ->
            match (isEmptyRow image row, isEmptyColumn image col) with
            | true, true -> multiplier * multiplier
            | true, false -> multiplier
            | false, true -> multiplier
            | _ -> 1L)

    let calculatePathCost (fromRow, fromColum) (toRow, toColumn) (costs: int64[,]) =
        let rowRange = [ (min fromRow toRow) + 1 .. max fromRow toRow ] |> List.map (fun row -> costs[row, fromColum])
        let colRange = [ (min fromColum toColumn) + 1 .. max fromColum toColumn ] |> List.map (fun col -> costs[fromRow, col])
        List.sum rowRange + List.sum colRange

    let findGalaxies image =
        Array2D.foldi (fun acc row col cell ->
            match cell with
            | Galaxy -> (row, col) :: acc
            | EmptySpace -> acc) [] image

    let solve multiplier (input: string list) =
        let image = parse input
        let costs = calculateCosts multiplier image
        let galaxies = findGalaxies image
        galaxies |> List.pairs |> Seq.sumBy (fun (a, b) -> calculatePathCost a b costs)

    [<AocSolver(2023, 11, Level = 1)>]
    let solve1 (input: string list) = solve 2L input

    [<AocSolver(2023, 11, Level = 2)>]
    let solve2 (input: string list) = solve 1000000L input
