module AdventOfCode.Solutions._2023.Day19

open AdventOfCode.Lib
open AdventOfCode.Lib.Solver
open FParsec

type Part = { X: int; M: int; A: int; S: int }
module Part =
    let pPart =
        pipe4
            (skipString "{x=" >>. pint32)
            (skipString ",m=" >>. pint32)
            (skipString ",a=" >>. pint32)
            (skipString ",s=" >>. pint32 .>> skipString "}")
            (fun x m a s -> { X = x; M = m; A = a; S = s })
    let parse str = parseOrDie pPart str

    let value part = part.X + part.M + part.A + part.S

type Predicate = SmallerThan of int | GreaterThan of int
module Predicate =
    let make chr value =
        match chr with
        | '<' -> SmallerThan value
        | '>' -> GreaterThan value
        | _ -> failwith "Invalid predicate"

type Field = X | M | A | S
module Field =
    let ofChar = function
        | 'x' -> X
        | 'm' -> M
        | 'a' -> A
        | 's' -> S
        | _ -> failwith "Invalid field"

type Condition = Predicate of (Field * Predicate) | Always
module Condition =
    let predicate fieldAndPredicate = Predicate fieldAndPredicate
    let always = Always

type Step = { Condition: Condition; Target: string }
module Step =
    let make condition target = { Condition = condition; Target = target }
    let makeFinalStep target = { Condition = Condition.always; Target = target }

type EndState = Accept | Reject

type Workflow = { Id: string; Steps: Step list }
module Workflow =
    let make id steps = { Id = id; Steps = steps }
    let pPredicate  = (pchar '<' <|> pchar '>') .>>. pint32  ||>> Predicate.make
    let pRegister = many1Chars (noneOf "<>:,{}\r\n")
    let pField = anyOf "xmas" |>> Field.ofChar
    let pCondition = pField .>>. pPredicate |>> Condition.predicate
    let pConditionStep = pCondition .>> skipChar ':' .>>. pRegister ||>> Step.make
    let pStep = attempt pConditionStep <|> (pRegister |>> Step.makeFinalStep)
    let pWorkflow = pRegister .>>. between (pchar '{') (pchar '}') (sepBy pStep (pchar ',')) ||>> make
    let parse str = parseOrDie pWorkflow str

    let execute part workflow =
        List.find (fun step ->
            match step.Condition with
            | Predicate (field, predicate) ->
                let partValue =
                    match field with
                    | X -> part.X
                    | M -> part.M
                    | A -> part.A
                    | S -> part.S
                match predicate with
                | SmallerThan targetValue -> partValue < targetValue
                | GreaterThan targetValue -> partValue > targetValue
            | Always -> true
        ) workflow.Steps
        |> (fun step -> step.Target)

    let executeMany part workflows =
        let map = List.fold (fun map workflow -> Map.add workflow.Id workflow map) Map.empty workflows
        let rec recurse workflowId =
            match workflowId with
            | "A" -> Accept
            | "R" -> Reject
            | _ ->
                let nextWorkflow = map[workflowId]
                let nextWorkflowId = execute part nextWorkflow
                recurse nextWorkflowId
        recurse "in"

type PartRange = { XR: int64 Range.t; MR: int64 Range.t; AR: int64 Range.t; SR: int64 Range.t }
module PartRange =
    let empty = { XR = Range.createEmpty (); MR = Range.createEmpty (); AR = Range.createEmpty (); SR = Range.createEmpty () }

    let split value field partRange =
        match field with
        | X -> let lo, hi = Range.splitAt value partRange.XR in ({ partRange with XR = lo }, { partRange with XR = hi })
        | M -> let lo, hi = Range.splitAt value partRange.MR in ({ partRange with MR = lo }, { partRange with MR = hi })
        | A -> let lo, hi = Range.splitAt value partRange.AR in ({ partRange with AR = lo }, { partRange with AR = hi })
        | S -> let lo, hi = Range.splitAt value partRange.SR in ({ partRange with SR = lo }, { partRange with SR = hi })

    let isEmpty partRange =
        Range.isEmpty partRange.XR || Range.isEmpty partRange.MR || Range.isEmpty partRange.AR || Range.isEmpty partRange.SR

    let combinations partRange =
        Range.length partRange.XR * Range.length partRange.MR *
            Range.length partRange.AR * Range.length partRange.SR

    let show range = $"x: {range.XR.Start}..{range.XR.End}, m: {range.MR.Start}..{range.MR.End}, a: {range.AR.Start}..{range.AR.End}, s: {range.SR.Start}..{range.SR.End} -> {combinations range}"

    let executeWorkflow partRanges workflow =
        List.fold (fun (remaining, result) step ->
            match step.Condition with
            | Always -> (empty, (step.Target, remaining) :: result) // Since we pass everything, what remains is empty.
            | Predicate (field, predicate) ->
                match predicate with
                | SmallerThan targetValue -> // We split on the target value, passing along everything larger to the next step in the workflow
                    let lo, hi = split targetValue field remaining
                    (hi, (step.Target, lo) :: result)
                | GreaterThan targetValue -> // We split on the target value, passing along everything smaller to the next step in the workflow
                    let lo, hi = split (int64 (targetValue + 1)) field remaining
                    (lo, (step.Target, hi) :: result)
            ) (partRanges, []) workflow.Steps
        |> snd

    let countCombinationsAccepted partRanges workflows =
        let map = List.fold (fun map workflow -> Map.add workflow.Id workflow map) Map.empty workflows
        let rec recurse remainingRanges count =
            match remainingRanges with
            | [] -> count
            | (workflowId, range) :: ranges ->
                match workflowId with
                | "A" -> recurse ranges (count + combinations range)
                | "R" -> recurse ranges count
                | _ ->
                    let workflow = map[workflowId]
                    recurse ((executeWorkflow range workflow) @ ranges) count
        recurse [("in", partRanges)] 0


let pInput = (sepEndBy Workflow.pWorkflow skipNewline) .>> skipNewline .>>. (sepEndBy Part.pPart skipNewline)
let parse input = parseOrDie pInput input


[<AocSolver(2023, 19, Level = 1)>]
let solve1 (input: string) =
    let workflows, parts = parse input
    parts
    |> List.filter (fun part -> Workflow.executeMany part workflows = Accept)
    |> List.sumBy Part.value

[<AocSolver(2023, 19, Level = 2)>]
let solve2 (input: string) =
    let workflows, _parts = parse input
    let parts = { XR = Range.create 1 4001
                  MR = Range.create 1 4001
                  AR = Range.create 1 4001
                  SR = Range.create 1 4001 }
    PartRange.countCombinationsAccepted parts workflows
