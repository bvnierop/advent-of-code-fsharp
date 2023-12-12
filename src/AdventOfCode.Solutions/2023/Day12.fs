module AdventOfCode.Solutions._2023.Day12

open AdventOfCode.Lib.Solver
open FParsec
open Microsoft.VisualBasic

type SpringState = Operational | Damaged | Unknown
module SpringState =
    let ofChar = function
        | '?' -> Unknown
        | '#' -> Damaged
        | _ -> Operational

type SpringRow = {
    States: SpringState list
    OperationalGroups: int list
}
module SpringRow =
    let unfold n row =
        let unfoldGroups n row =
            [1..n - 1]
            |> List.fold (fun acc _ -> row @ acc) row

        let unfoldStates n row =
            [1..n - 1]
            |> List.fold (fun acc _ -> row @ (Unknown :: acc)) row

        { States = row.States |> unfoldStates n;
          OperationalGroups = row.OperationalGroups |> unfoldGroups n }

let pOperationalGroups = sepEndBy pint32 (pchar ',')
let pRow = manyTill anyChar spaces1 |>> List.map SpringState.ofChar
let pLine = pRow .>>. pOperationalGroups
let parseLine line =
    let states, groups = parseOrDie pLine line
    { States = states; OperationalGroups = groups }

let tryProcessGroup record =
    let rec recurse states remainingSize =
        if remainingSize = 0 then
            match states with
            | [] -> Some { States = states; OperationalGroups = List.tail record.OperationalGroups }
            | (Operational | Unknown) :: remainingStates -> Some { States = remainingStates; OperationalGroups = List.tail record.OperationalGroups }
            | _ -> None
        else
            match states with
            | (Damaged | Unknown) :: remainingStates -> recurse remainingStates (remainingSize - 1)
            | _ -> None
    if List.isEmpty record.OperationalGroups then None
    else recurse record.States (List.head record.OperationalGroups)

let numberOfPossibilities record =
    let mutable memo = Map.empty
    let rec recurse record =
        if not (Map.containsKey record memo) then
            let result =
                match record.States with
                | [] ->
                    if List.isEmpty record.OperationalGroups then 1L
                    else 0L
                | Operational :: remainingStates -> recurse { States = remainingStates; OperationalGroups = record.OperationalGroups }
                | Damaged :: _remainingStates ->
                    match tryProcessGroup record with
                    | None -> 0L
                    | Some recordAfterDamagedGroup -> recurse recordAfterDamagedGroup
                | Unknown :: remainingStates ->
                    let damagedCount =
                        match tryProcessGroup record with
                        | None -> 0L
                        | Some recordAfterDamagedGroup -> recurse recordAfterDamagedGroup
                    recurse { States = remainingStates; OperationalGroups = record.OperationalGroups } + damagedCount
            memo <- Map.add record result memo
        memo[record]
    recurse record

[<AocSolver(2023, 12, Level = 1)>]
let solve1 (input: string list) =
    List.map parseLine input
    |> List.sumBy numberOfPossibilities

[<AocSolver(2023, 12, Level = 2)>]
let solve2 (input: string list) =
    List.map parseLine input
    |> List.map (SpringRow.unfold 5)
    |> List.sumBy numberOfPossibilities
