module AdventOfCode.Solutions._2023.Day14

open System.Drawing
open AdventOfCode.Lib.Solver
open System

module Grid2D =
    type 'a t = 'a array2d

    let make (init: seq<#seq<'a>>) = array2D init

    type Row = Row of int
    let inline up (Row row) = Row (row - 1)
    let inline down (Row row) = Row (row + 1)

    type Column = Column of int
    let inline right (Column column) = Column (column + 1)
    let inline left (Column column) = Column (column - 1)

    let inline height (grid: 'a t) = Array2D.length1 grid
    let inline width (grid: 'a t) = Array2D.length2 grid

    let inline firstRow (_grid: 'a t) = Row 0
    let inline lastRow grid = Row (height grid - 1)
    let inline firstColumn (_grid: 'a t) = Column 0
    let inline lastColumn grid = Column (width grid - 1)

    let inline isInside (Row row) (Column col) (grid: 'a t) =
        let height = height grid
        let width = width grid
        0 <= row && row < height && 0 <= col && col < width

    let inline private get (Row row) (Column col) (grid: 'a t) = grid[row, col]

    let private tryFind predicate row changeRow col changeCol grid =
        let rec recurse row' col' =
            if not (isInside row' col' grid) then None
            elif predicate (get row' col' grid) then Some (row', col')
            else recurse (changeRow row') (changeCol col')
        recurse row col

    let tryFindUp predicate row col (grid: 'a t) = tryFind predicate row up col id grid
    let tryFindDown predicate row col (grid: 'a t) = tryFind predicate row down col id grid
    let tryFindLeft predicate row col (grid: 'a t) = tryFind predicate row id col left grid
    let tryFindRight predicate row col (grid: 'a t) = tryFind predicate row id col right grid

    let swap (Row row1) (Column column1) (Row row2) (Column column2) (grid: 'a t) =
        let temp = grid[row1, column1]
        grid[row1, column1] <- grid[row2, column2]
        grid[row2, column2] <- temp
        grid

    let findAll predicate (grid: 'a t) =
        Array2D.foldi (fun acc row col value ->
            if predicate value then (Row row, Column col) :: acc
            else acc) [] grid
        |> List.rev

    let show (grid: 'a t) =
        let height = height grid
        let width = width grid
        let sb = System.Text.StringBuilder()
        for row in 0 .. height - 1 do
            for col in 0 .. width - 1 do
                sb.Append(grid[row, col]) |> ignore
            sb.AppendLine() |> ignore
        sb.ToString()

let tilt finder sorter initialRowChange initialColChange notFoundRow notFoundColumn foundRowChange foundColumnChange (grid: char Grid2D.t) =
    Grid2D.findAll (fun c -> c = 'O') grid
    |> List.sortBy sorter
    |> List.fold (fun grid (row, col) ->
           let pos = finder (fun c -> c <> '.') (initialRowChange row) (initialColChange col) grid
           match pos with
           | None -> Grid2D.swap row col (notFoundRow row) (notFoundColumn col) grid
           | Some (r,c) -> Grid2D.swap row col (foundRowChange r) (foundColumnChange c) grid) grid


let tiltUp (grid: char Grid2D.t) =
    tilt Grid2D.tryFindUp fst
        Grid2D.up id (fun _r -> Grid2D.firstRow grid) id Grid2D.down id grid

let tiltDown (grid: char Grid2D.t) =
    tilt Grid2D.tryFindDown (fun (Grid2D.Row r, _c) -> -r)
        Grid2D.down id (fun _r -> Grid2D.lastRow grid) id Grid2D.up id grid

let tiltLeft (grid: char Grid2D.t) =
    tilt Grid2D.tryFindLeft snd
        id Grid2D.left id (fun _c -> Grid2D.firstColumn grid) id Grid2D.right grid

let tiltRight(grid: char Grid2D.t) =
    tilt Grid2D.tryFindRight (fun (_r, Grid2D.Column c) -> -c)
        id Grid2D.right id (fun _c -> Grid2D.lastColumn grid) id Grid2D.left grid

let tiltCycle grid = grid |> tiltUp |> tiltLeft |> tiltDown |> tiltRight

let calculateTiltCycleCount grid =
    let rec findCycle grid seen stepCount =
        if Map.containsKey grid seen then
            let runUp = Map.find grid seen
            let cycle = stepCount - runUp
            runUp, cycle
        else
            let seen' = Map.add (Array2D.copy grid) stepCount seen
            let grid' = tiltCycle grid
            findCycle grid' seen' (stepCount + 1)
    let runUp, cycleLength = findCycle grid Map.empty 0
    let totalSteps = 1000000000
    let remainingSteps = totalSteps - runUp
    let rest = remainingSteps % cycleLength
    runUp + rest

let totalWeight (grid: char Grid2D.t) =
    let allRocks = Grid2D.findAll (fun c -> c = 'O') grid
    let height = Grid2D.height grid
    allRocks
    |> List.sumBy (fun (Grid2D.Row row, _col) -> height - row)


module Day14 =
    [<AocSolver(2023, 14, Level = 1)>]
    let solve1 (input: string list) =
        let grid = Grid2D.make input
        let afterMoving = tiltUp grid
        let weight = totalWeight afterMoving
        weight

    [<AocSolver(2023, 14, Level = 2)>]
    let solve2 (input: string list) =
        let grid = Grid2D.make input
        let cycles = calculateTiltCycleCount (Array2D.copy grid)
        let afterTilting =
            [1..cycles]
            |> List.fold (fun grid _ -> tiltCycle grid) grid
        let weight = totalWeight afterTilting
        weight
