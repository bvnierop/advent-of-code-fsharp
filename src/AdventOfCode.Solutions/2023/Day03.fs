namespace AdventOfCode.Solutions._2023

open AdventOfCode.Lib.Solver
open System

module Day03 =
    type Cell =
    | Digit of int
    | PotentialGear
    | OtherPart
    | Number of (int * int)
    | Nothing

    let parseLine (line: string) =
        line.ToCharArray()
        |> Array.map (function
            | c when Char.IsDigit(c) -> Digit (Int32.Parse(c.ToString()))
            | '.' -> Nothing
            | '*' -> PotentialGear
            | _ -> OtherPart)

    let convertDigits (startAt: int) (digits: Cell array) =
        let rec lookAhead (index: int) (currentNumber: int) =
            match Array.getDefault Nothing index digits with
            | Digit d -> lookAhead (index + 1) (currentNumber * 10 + d)
            | _ -> currentNumber

        let mappingFunction (index: int, currentNumber: Cell option, numberCount: int) cell =
            match cell with
            | Digit d ->
                match currentNumber with
                | Some n -> n, (index + 1, currentNumber, numberCount)
                | None ->
                    let number = lookAhead (index + 1) d
                    Number (number, numberCount), (index + 1, Some (Number (number, numberCount)), numberCount + 1)
            | _ -> cell, (index + 1, None, numberCount)

        let mapped, (_, _, count) = Array.mapFold mappingFunction (0, None, startAt) digits
        mapped, count

    let parse lines =
        List.map parseLine lines
        |> List.mapFold convertDigits 0
        |> fst
        |> List.toArray

    let findPartIndices filter schematic =
        schematic
        |> Array.indexed
        |> Array.collect (fun (y, row) ->
            row |> Array.indexed |> Array.filter (fun (_, cell) -> List.contains cell filter)
            |> Array.map (fun (x, _) -> (y, x)))

    let neighbouringNumbers schematic (partY, partX) =
        (partY, partX, schematic)
        |||> Array.neighbouringValues8
        |> Seq.choose (function
            | Number (n, i) -> Some (n, i)
            | _ -> None)
        |> Seq.distinct

    [<AocSolver(2023, 3, Level = 1)>]
    let solve1 (input: string list) =
        let schematic = input |> parse
        let parts = findPartIndices [PotentialGear; OtherPart] schematic

        parts
        |> Array.map (neighbouringNumbers schematic)
        |> Array.sumBy (Seq.sumBy fst)

    [<AocSolver(2023, 3, Level = 2)>]
    let solve2 (input: string list) =
        let schematic = input |> parse
        let parts = findPartIndices [PotentialGear] schematic

        parts
        |> Array.map (neighbouringNumbers schematic)
        |> Array.filter (fun s -> Seq.length s = 2)
        |> Array.sumBy (Seq.map fst >> Seq.fold (*) 1)
