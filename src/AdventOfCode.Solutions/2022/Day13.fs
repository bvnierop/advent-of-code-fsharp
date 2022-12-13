namespace AdventOfCode.Solutions._2022

open AdventOfCode.Lib.Solver
open System
open FParsec

module Day13 =
    [<CustomComparison; CustomEquality>]
    type Expr<'a when 'a: comparison> =
        Value of 'a | List of 'a Expr list
        
        static member compare a b =
            match (a, b) with
            | (Value aVal, Value bVal) -> Operators.compare aVal bVal
            | (List aList, List bList) -> Operators.compare aList bList
            | (List _, Value _) -> compare a (List [b])
            | (Value _, List _) -> compare (List [a]) b
            
        interface IComparable with
            member a.CompareTo b =
                match b with
                | :? Expr<'a> as e -> Expr<'a>.compare a e
                | _ -> invalidArg "b" "Cannot compare values of different types"
                
        override this.Equals other =
            match other with
            | :? Expr<'a> as e -> Expr<'a>.compare this e = 0
            | _ -> false
          
        override this.GetHashCode() = hash (this)
            
    let pExpr, pExprImpl = createParserForwardedToRef()
    let pList = between (pchar '[') (pchar ']') (sepBy pExpr (pchar ',')) |>> List
    let pValue = pint32 |>> Value
    pExprImpl.Value <- pValue <|> pList
    
    let parseList = List.map (parseOrDie pExpr)
    let rightOrder expressions = expressions = List.sort expressions
    
    [<AocSolver(2022, 13, Level = 1)>]
    let solve1 (input: string list) =
        input |> List.splitOnExclusive String.isNullOrEmpty
        |> List.map (parseList >> rightOrder)
        |> List.indexed
        |> List.filter (fun (_, b) -> b)
        |> List.map (fst >> ((+) 1))
        |> List.sum
        
    [<AocSolver(2022, 13, Level = 2)>]
    let solve2 (input: string list) =
        let dividers = ["[[2]]";"[[6]]"] |> parseList
        input |> List.reject String.isNullOrEmpty
        |> parseList
        |> List.append dividers
        |> List.sort
        |> List.indexed
        |> List.filter (fun (_, e) -> List.contains e dividers)
        |> List.map (fst >> ((+) 1))
        |> List.fold (*) 1