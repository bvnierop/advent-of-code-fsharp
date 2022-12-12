namespace AdventOfCode.Solutions._2022

open AdventOfCode.Lib.Solver
open System

module Day11 =
    
    type Monkey = {
        Items: int64 list
        Operation: (string * int);
        Test: int;
        IfTrue: int;
        IfFalse: int;
    }
    let parseOperation (str: string) =
        let finalTwoElements =
            str |> String.split |> Array.rev |> Array.take 2
        match finalTwoElements with
        | [|num; op|] ->
            if num = "old" then ("exp", 2)
            else (op, Int32.parse num)
        | _ -> failwith $"Failed to parse operation: {str}"
        
    let parseMonkey (input: string list) =
        let lastAsInt str = str |> String.split |> Array.last |> Int32.parse
        match input |> List.skip 1 with
        | [items;operation;test;ifTrue;ifFalse] ->
            {
                Items = items |> String.splitOn [|':'|] |> Array.last |> String.splitOn [|','|] |> Array.map Int64.parse |> Array.toList;
                Operation = parseOperation operation;
                Test = lastAsInt test;
                IfTrue = lastAsInt ifTrue;
                IfFalse = lastAsInt ifFalse;
            }
        | _ -> failwith "Failed to parse monkey"
        
    let parseMonkeys (input: string list) =
        input |> List.splitOnExclusive String.isNullOrWhiteSpace
        |> List.map parseMonkey
        |> List.toArray
        
    let solve rounds worryControlFn monkeys =
        let mutable counts = Array.create (Array.length monkeys) 0L
        
        let processMonkey index (monkeys: Monkey array) =
            let monkey = monkeys[index]
            let mutable m = monkeys
            let throwItem n item monkeys =
                monkeys
                |> Array.updateAt n { monkeys[n] with Items = List.append monkeys[n].Items [item] }
                
            let calculateWorry (operation: (string * int)) item =
                match operation with
                | ("*", n) -> item * int64 n
                | ("+", n) -> item + int64 n
                | ("exp", _) -> item * item
                | _ -> failwith "We messed up big time here"
                
            for item in monkey.Items do
                let mutable worry = (calculateWorry monkey.Operation item)
                worry <- worryControlFn worry
                assert(worry >= 0L)
                
                // Note: mark that the monkey has inspected an item
                if worry % int64 monkey.Test = 0 then m <- throwItem monkey.IfTrue worry m
                else m <- throwItem monkey.IfFalse worry m
                
            counts <- Array.updateAt index (counts[index] + (int64)(List.length m[index].Items)) counts
            Array.updateAt index { m[index] with Items = [] } m
        
        let rec round n monkeys =
            if n = Array.length monkeys then monkeys
            else
                round <| n + 1 <| processMonkey n monkeys
            
        let mutable m = monkeys
        for i = 1 to rounds do
            m <- round 0 m
            
        counts |> Array.sortDescending |> Array.take 2 |> Array.fold (*) 1L
        
    [<AocSolver(2022, 11, Level = 1)>]
    let solve1 (input: string list) =
        let monkeys = parseMonkeys input
        solve 20 (fun w -> w / 3L) monkeys
        
    [<AocSolver(2022, 11, Level = 2)>]
    let solve2 (input: string list) =
        let monkeys = parseMonkeys input
        let product = monkeys |> Array.map (fun m -> int64 m.Test) |> Array.fold (*) 1L
        solve 10000 (fun w -> w % product) monkeys
