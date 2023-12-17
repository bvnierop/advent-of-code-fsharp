module AdventOfCode.Solutions._2023.Day15

open AdventOfCode.Lib.Solver
open FParsec

let pStep = manyChars (noneOf ",\r\n")
let pInitializationSequence = sepBy pStep (pchar ',')
let parse input = parseOrDie pInitializationSequence input

let HASH(input: string) =
    Seq.map int input
    |> Seq.fold (fun hashValue chr ->
        ((chr + hashValue) * 17) % 256) 0

type Operation = Set of int | Remove
type Instruction = { Label : string; Operation : Operation }
module Instruction =
    let make str op = { Label = str; Operation = op }

    let pRemoveOperation = pchar '-' >>% Remove
    let pSetOperation = pchar '=' >>. pint32 |>> Set
    let pOperation = pSetOperation <|> pRemoveOperation
    let pLabel = manyChars asciiLetter
    let pInstruction = pLabel .>>. pOperation ||>> make
    let pInstructions = sepBy pInstruction (pchar ',')

    let applyToHashMap hashmap instr =
        let update (label, newLength) = List.map (fun (l, length) -> if l = label then (l, newLength) else (l, length))

        match instr.Operation with
        | Set focusLength ->
            Map.change
                (HASH instr.Label)
                (fun currentValue ->
                    let currentValue' = Option.defaultValue [] currentValue
                    if List.exists (fun (label, _) -> label = instr.Label) currentValue' then
                        Some (update (instr.Label, focusLength) currentValue')
                    else
                        Some ((instr.Label, focusLength) :: currentValue'))
                hashmap
        | Remove ->
            Map.change
                (HASH instr.Label)
                (function
                 | None -> None
                 | Some currentValue -> Some (List.reject (fun (label, _) -> label = instr.Label) currentValue))
                 hashmap

    let parse str =
       parseOrDie pInstructions str

let focusingPower hashmap =
    Map.fold (fun sum boxNumber lenses ->
        lenses
        |> List.rev
        |> List.indexed
        |> List.fold (fun innerSum (index, (_label, focusLength)) ->
            innerSum + ((1 + boxNumber) * (index + 1) * focusLength)) sum) 0 hashmap

[<AocSolver(2023, 15, Level = 1)>]
let solve1 (input: string) =
    input
    |> parse
    |> List.sumBy HASH

[<AocSolver(2023, 15, Level = 2)>]
let solve2 (input: string) =
    input
    |> Instruction.parse
    |> List.fold Instruction.applyToHashMap Map.empty
    |> focusingPower
