namespace AdventOfCode.Solutions._2022

open AdventOfCode.Lib.Solver
open System

module Day11 =
    
    type Monkey = {
        Items: int64 list
        Operation: (string * int64);
        Test: int64;
        IfTrue: int;
        IfFalse: int;
    }
    let parseOperation (str: string) =
        let finalTwoElements =
            str |> String.split |> Array.rev |> Array.take 2
        match finalTwoElements with
        | [|num; op|] ->
            if num = "old" then ("exp", 2L)
            else (op, Int64.parse num)
        | _ -> failwith $"Failed to parse operation: {str}"
        
    let parseMonkey (input: string list) =
        let lastAsInt64 str = str |> String.split |> Array.last |> Int64.parse
        let lastAsInt str = str |> String.split |> Array.last |> Int32.parse
        match input |> List.skip 1 with
        | [items;operation;test;ifTrue;ifFalse] ->
            {
                Items = items |> String.splitOn [|':'|] |> Array.last |> String.splitOn [|','|] |> Array.map Int64.parse |> Array.toList;
                Operation = parseOperation operation;
                Test = lastAsInt64 test;
                IfTrue = lastAsInt ifTrue;
                IfFalse = lastAsInt ifFalse;
            }
        | _ -> failwith "Failed to parse monkey"
        
    let parseMonkeys (input: string list) =
        input |> List.splitOnExclusive String.isNullOrWhiteSpace
        |> List.map parseMonkey
        |> List.toArray
        
    [<AocSolver(2022, 11, Level = 1)>]
    let solve1 (input: string list) =
        let monkeys = parseMonkeys input
        // First parse each monkey
        //    Splitting the input on empty lines
        //    Discard the first line (Monkey 0:)
        //    The second line is the items, comma separated after a colon
        //    Third line is the operation, we're only interested in the last two elements
        //    Fourth line: test = divisible by last element
        //      If true -> last element
        //      If false -> last element
        // Simulate a round
        //   Note to self: Figure out later if we can use fold or scan or something like that
        //   For each monkey in order
        //    If it does have items
        //      Process the monkey
        //      Continue simulating the round with the next monkey
        
        let mutable counts = Array.create (Array.length monkeys) 0
        
        let processMonkey index (monkeys: Monkey array) =
            let monkey = monkeys[index]
            let mutable m = monkeys
            let throwItem n item monkeys =
                monkeys
                |> Array.updateAt n { monkeys[n] with Items = List.append monkeys[n].Items [item] }
                
            let calculateWorry (operation: (string * int64)) item =
                match operation with
                | ("*", n) -> item * n
                | ("+", n) -> item + n
                | ("exp", _) -> item * item
                | _ -> failwith "We messed up big time here"
                
            for item in monkey.Items do
                let worry = (calculateWorry monkey.Operation item) / 3L
                // Note: mark that the monkey has inspected an item
                if worry % monkey.Test = 0 then m <- throwItem monkey.IfTrue worry m
                else m <- throwItem monkey.IfFalse worry m
                
            counts <- Array.updateAt index (counts[index] + (List.length m[index].Items)) counts
            Array.updateAt index { m[index] with Items = [] } m
                
        //       Increase worry level based on formula
        //       Divide worry level by three
        //       Mark that the monkey has inspected an item
        //       Perform test -> Throw item
        
        let rec round n monkeys =
            if n = Array.length monkeys then monkeys
            else
                round <| n + 1 <| processMonkey n monkeys
            
        let mutable m = monkeys
        for i = 1 to 20 do
            m <- round 0 m
            
        // Processing a monkey
        //    For each item in the monkey's possession
        //       Increase worry level based on formula
        //       Mark that the monkey has inspected an item
        //       Divide worry level by three
        //       Perform test -> Throw item
        // Simulate 20 rounds
        // Sort monkeys by inspected items
        // Take the top 2
        // Multiply them together
        counts |> Array.sortDescending |> Array.take 2 |> Array.fold (*) 1
        
    [<AocSolver(2022, 11, Level = 2)>]
    let solve2 (input: string list) =
        let monkeys = parseMonkeys input
        
        let mutable counts = Array.create (Array.length monkeys) 0L
        let product = monkeys |> Array.map (fun m -> m.Test) |> Array.fold (*) 1L
        
        let processMonkey index (monkeys: Monkey array) =
            let monkey = monkeys[index]
            let mutable m = monkeys
            let throwItem n item monkeys =
                monkeys
                |> Array.updateAt n { monkeys[n] with Items = List.append monkeys[n].Items [item] }
                
            let calculateWorry (operation: (string * int64)) item =
                match operation with
                | ("*", n) -> item * n
                | ("+", n) -> item + n
                | ("exp", _) -> item * item
                | _ -> failwith "We messed up big time here"
                
            for item in monkey.Items do
                let mutable worry = (calculateWorry monkey.Operation item)
                worry <- worry % product
                assert(worry >= 0L)
                
                // Note: mark that the monkey has inspected an item
                if worry % monkey.Test = 0 then m <- throwItem monkey.IfTrue worry m
                else m <- throwItem monkey.IfFalse worry m
                
            counts <- Array.updateAt index (counts[index] + (int64)(List.length m[index].Items)) counts
            Array.updateAt index { m[index] with Items = [] } m
        
        let rec round n monkeys =
            if n = Array.length monkeys then monkeys
            else
                round <| n + 1 <| processMonkey n monkeys
            
        let mutable m = monkeys
        for i = 1 to 10000 do
            m <- round 0 m
            
        counts |> Array.sortDescending |> Array.take 2 |> Array.fold (*) 1L
