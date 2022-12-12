namespace AdventOfCode.Solutions._2022

open AdventOfCode.Lib.Solver
open System

module Day11 =
    open FParsec
    
    type Monkey = {
        Items: int64 list
        Operation: int64 -> int64
        Test: int;
        IfTrue: int;
        IfFalse: int;
    }
    
    let pSkipEndOfLine = skipNewline <|> eof
    let pIdentifier = pstring "Monkey " >>. pint32 >>. pstring ":" .>> pSkipEndOfLine
    let pItems = (spaces >>. pstring "Starting items: ") >>. (sepBy <| pint64 <| pstring ", ") .>> pSkipEndOfLine
    let pOp = spaces >>. pstring "Operation: new = old " >>. choice [
        pstring "* old" >>% (fun i -> i * i) .>> pSkipEndOfLine;
        pstring "* " >>. pint64 |>> (fun o -> (fun i -> i * o)) .>> pSkipEndOfLine;
        pstring "+ " >>. pint64 |>> (fun o -> (fun i -> i + o)) .>> pSkipEndOfLine
    ]
    let pTest = spaces >>. pstring "Test: divisible by " >>. pint32 .>> pSkipEndOfLine
    let pTrue = spaces >>. pstring "If true: throw to monkey " >>. pint32 .>> pSkipEndOfLine
    let pFalse = spaces >>. pstring "If false: throw to monkey " >>. pint32 .>> pSkipEndOfLine
    let pMonkey =
        pIdentifier
        >>. pipe5 pItems pOp pTest pTrue pFalse (fun items op test t f -> {
            Items = items;
            Operation = op;
            Test = test;
            IfTrue = t;
            IfFalse = f;
        })
    let pMonkeys = sepEndBy pMonkey pSkipEndOfLine |>> List.toArray
    
    let parse str = parseOrDie pMonkeys str
        
    let solve rounds worryControlFn monkeys =
        let processMonkey index counts (monkeys: Monkey array) =
            let monkey = monkeys[index]
            let throwItem n item monkeys =
                monkeys
                |> Array.updateAt n { monkeys[n] with Items = List.append monkeys[n].Items [item] }
                
            let monkeysAfterThrowing =
                List.fold (fun monkeys item ->
                    let worry = worryControlFn <| monkey.Operation item
                    assert(worry >= 0L)
                    
                    if worry % int64 monkey.Test = 0 then throwItem monkey.IfTrue worry monkeys
                    else throwItem monkey.IfFalse worry monkeys) monkeys monkey.Items
                
            (counts |> Array.updateAt index (counts[index] + (int64)(List.length monkey.Items)),
            Array.updateAt index { monkeysAfterThrowing[index] with Items = [] } monkeysAfterThrowing)
        
        let rec round n counts monkeys =
            if n = Array.length monkeys then (counts, monkeys)
            else
                round <| n + 1 <|| processMonkey n counts monkeys
            
        let counts = Array.create (Array.length monkeys) 0L
        
        [1..rounds]
        |> List.fold (fun (counts, monkeys) _ -> round 0 counts monkeys) (counts, monkeys)
        |> fst
        |> Array.sortDescending |> Array.take 2 |> Array.fold (*) 1L
        
    [<AocSolver(2022, 11, Level = 1)>]
    let solve1 (input: string) =
        let monkeys = parse input
        solve 20 (fun w -> w / 3L) monkeys
        
    [<AocSolver(2022, 11, Level = 2)>]
    let solve2 (input: string) =
        let monkeys = parse input
        let product = monkeys |> Array.map (fun m -> int64 m.Test) |> Array.fold (*) 1L
        solve 10000 (fun w -> w % product) monkeys
