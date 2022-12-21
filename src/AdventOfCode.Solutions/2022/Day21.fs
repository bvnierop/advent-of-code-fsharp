module AdventOfCode.Solutions._2022.Day21

open AdventOfCode.Lib.Solver
open System
open FParsec

type Expr = Value of int64 option | Expr of (string * char * string) | Guess

let pPrefix = anyString 4 .>> skipString ": "
let pValue = pint64 |>> (Some >> Value)
let pExpr =
    (anyString 4)
    .>>. (spaces >>. anyChar .>> spaces)
    .>>. (anyString 4) |>> (fun ((arg1, op), arg2) -> Expr (arg1, op, arg2))
let pLine = pPrefix .>>. (pExpr <|> pValue)

let parse input =
    input |> List.map (parseOrDie pLine)
    |> List.fold (fun cache (name, expr) -> Map.add name expr cache) Map.empty
    
let eval op arg1 arg2 =
    match op with
    | '+' -> arg1 + arg2
    | '-' -> arg1 - arg2
    | '*' -> arg1 * arg2
    | '/' -> arg1 / arg2
    | _ -> failwith $"Invalid op: {op}"
    
let evalOpt op arg1 arg2 =
    match (arg1, arg2) with
    | (Some a, Some b) -> Some <| eval op a b
    | _ -> None
    
let rec compute monkey (monkeys: Map<string, Expr>) =
    match monkeys[monkey] with
    | Value i -> (i, monkeys)
    | Expr (arg1, op, arg2) ->
        let a, m = compute arg1 monkeys
        let b, m2 = compute arg2 m
        let res = evalOpt op a b
        res, m2 |> Map.add monkey (Value res)
    | Guess -> (None, Map.add monkey (Value None) monkeys)
    
// In order to guess we need to know the expression up to this point
    
[<AocSolver(2022, 21, Level = 1)>]
let solve1 (input: string list) =
    input
    |> parse
    |> compute "root"
    |> fst
    |> Option.defaultValue -1
    
    
/// Pass in the form target = arg OP x`
let evalInverseA op target arg =
    match op with
    | '=' -> arg
    | '+' -> target - arg
    | '-' -> arg - target
    | '*' -> target / arg
    | '/' -> target / arg
    | _ -> failwith $"Unknown operator: {op}"
    
/// Pass in the form target = x OP arg`
let evalInverseB op target arg =
    match op with
    | '=' -> arg
    | '+' -> target - arg
    | '-' -> target + arg
    | '*' -> target / arg
    | '/' -> target * arg
    | _ -> failwith $"Unknown operator: {op}"
    
let convertRoot root =
    match root with
    | Value _ -> root
    | Guess -> root
    | Expr (arg1, _op, arg2) -> Expr (arg1, '=', arg2)
    
let rec fix monkey (known: Map<string, int64 option>) (monkeys: Map<string, Expr>) target =
    match known[monkey] with
    | Some i -> i
    | None -> // Monkeys must be one of Expr or Guess
        match monkeys[monkey] with
        | Guess -> target
        | Expr (a, op, b) ->
            match (known[a], known[b]) with
            | (Some a, None) -> // We don't know b
                fix b known monkeys (evalInverseA op target a)
            | (None, Some b) ->
                fix a known monkeys (evalInverseB op target b)
            | None, None -> failwith "Both sides cannot be None"
            | (Some _, Some _) -> failwith "Both sides cannot be Some"
        | _ -> failwith "Nope. Not possible"
    
[<AocSolver(2022, 21, Level = 2)>]
let solve2 (input: string list) =
    let monkeys =
        input |> parse
        |> Map.add "humn" Guess
    let rootMonkey = monkeys["root"]
    let monkeys = monkeys |> Map.add "root" (convertRoot rootMonkey)
    let _, known = compute "root" monkeys
    let knownValues =
        known |> Map.map (fun monkey expr -> 
                              match expr with
                              | (Value x) -> x
                              | _ -> failwith "Everything should be a value")
    fix "root" knownValues monkeys -1