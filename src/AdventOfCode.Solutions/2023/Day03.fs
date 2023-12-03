namespace AdventOfCode.Solutions._2023

open AdventOfCode.Lib.Solver
open System

module Day03 =
    type Cell =
    | Digit of int
    | PotentialGear
    | OtherPart
    | Number of (int * int)

    let parseLine (line: string) =
        line.ToCharArray()
        |> Array.map (function
            | c when Char.IsDigit(c) -> Some (Digit (Int32.Parse(c.ToString())))
            | '.' -> None
            | '*' -> Some PotentialGear
            | _ -> Some OtherPart)

    let convertDigits (startAt: int) (digits: Cell option array) =
        let rec lookAhead (index: int) (currentNumber: int) =
            match Array.getDefault None index digits with
            | Some (Digit d) -> lookAhead (index + 1) (currentNumber * 10 + d)
            | _ -> currentNumber

        let mappingFunction (index: int, currentNumber: Cell option, numberCount: int) cell =
            match cell with
            | Some (Digit d) ->
                match currentNumber with
                | Some _ -> currentNumber, (index + 1, currentNumber, numberCount)
                | None ->
                    let number = lookAhead (index + 1) d
                    Some (Number (number, numberCount)), (index + 1, Some (Number (number, numberCount)), numberCount + 1)
            | _ -> cell, (index + 1, None, numberCount)

        let mapped, (_, _, count) = Array.mapFold mappingFunction (0, None, startAt) digits
        mapped, count

    let parse lines =
        List.map parseLine lines
        |> List.mapFold convertDigits 0
        |> fst
        |> List.toArray

    let findParts filter schematic =
        schematic
        |> Array.indexed
        |> Array.collect (fun (y, row) ->
            row |> Array.indexed |> Array.filter (fun (_, cell) -> List.contains cell filter)
            |> Array.map (fun (x, _) -> (y, x)))


    [<AocSolver(2023, 3, Level = 1)>]
    let solve1 (input: string list) =
        let schematic = input |> parse
        let parts = findParts [Some PotentialGear; Some OtherPart] schematic
        let neighboursByPart =
            parts
            |> Array.map (fun (y, x) -> Array.neighbours8 y x schematic |> Array.ofSeq)
        neighboursByPart
        |> Array.map (fun pts ->
            pts
            |> Array.map (fun (y, x) -> Array.getDefault2D None y x schematic)
            |> Array.choose (function
                | Some (Number (n, i)) -> Some (n, i)
                | _ -> None)
            |> Set.ofArray
            |> Set.sumBy fst)
        |> Array.sum

    [<AocSolver(2023, 3, Level = 2)>]
    let solve2 (input: string list) =
        let schematic = input |> parse
        let parts = findParts [Some PotentialGear] schematic
        let neighboursByPart =
            parts
            |> Array.map (fun (y, x) -> Array.neighbours8 y x schematic |> Array.ofSeq)
        neighboursByPart
        |> Array.map (fun pts ->
            pts
            |> Array.map (fun (y, x) -> Array.getDefault2D None y x schematic)
            |> Array.choose (function
                | Some (Number (n, i)) -> Some (n, i)
                | _ -> None)
            |> Set.ofArray)
       |> Array.filter (fun s -> Set.count s = 2)
       |> Array.map (Set.map fst >> Set.fold (*) 1)
       |> Array.sum
