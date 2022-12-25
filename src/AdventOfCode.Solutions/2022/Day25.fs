namespace AdventOfCode.Solutions._2022

open AdventOfCode.Lib.Solver
open System

module Day25 =
    
    let convertDigit digit =
        match digit with
        | '-' -> -1L | '=' -> -2L | c -> Int64.parseChr c
    let decimalFromSNAFU (number: string) =
        number |> Seq.map convertDigit
        |> Seq.fold (fun dec digit -> digit + dec * 5L) 0L
    let decrSNAFU digit =
        match digit with
        | '2' -> '1' | '1' -> '0' | '0' -> '-' | '-' -> '='
        | _ -> failwith $"Cannot decr {digit}"
        
    let decimalToSNAFU number =
        let rec largeNum snafu =
            if (decimalFromSNAFU snafu) >= number then snafu
            else largeNum <| "2" + snafu
            
        let rebuild (leftDigits: char list) (rightDigits: char list) =
            rightDigits
            |> List.append (List.rev leftDigits)
            |> String.Concat
            
        let rec reduceNum (leftDigits: char list) (rightDigits: char list) =
            let rebuilt = rebuild leftDigits rightDigits
            if decimalFromSNAFU rebuilt = number then rebuilt
            else
                match rightDigits with
                | [] -> failwith "can't be empty"
                | d::ds ->
                    match d with
                    | '=' -> reduceNum <| d :: leftDigits <| ds
                    | _ ->
                        let decreased = decrSNAFU d
                        if decimalFromSNAFU (rebuild leftDigits <| decreased :: ds) < number then
                            reduceNum <| d :: leftDigits <| ds
                        else reduceNum <| leftDigits <| decreased :: ds
            
        largeNum "2"
        |> Seq.toList
        |> reduceNum []
            
    [<AocSolver(2022, 25, Level = 1)>]
    let solve1 (input: string list) =
        input |> List.map decimalFromSNAFU
        |> List.sum
        |> decimalToSNAFU
        
    [<AocSolver(2022, 25, Level = 2)>]
    let solve2 (input: string list) =
        "*"