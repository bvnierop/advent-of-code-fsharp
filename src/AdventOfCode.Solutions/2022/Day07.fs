namespace AdventOfCode.Solutions._2022

open AdventOfCode.Lib.Solver
open System

module Day07 =
    let rec updateSizes pwd size sizes =
        match pwd with
        | [] -> sizes   
        | _::xs -> 
            let updated = 
                sizes
                |> Map.change pwd (fun i -> Some (Option.defaultValue 0 i + size))
            updateSizes xs size updated
            
    let calculateSizes (lines: string list) =
        let handleLine (pwd, sizes) (line: string) = 
            match line.Split(" ") with
            | [|"$"; "cd"; "/"|] -> (["/"], sizes)
            | [|"$"; "cd"; ".."|] -> (List.tail pwd, sizes)
            | [|"$"; "cd"; dirName|] -> (dirName :: pwd, sizes)
            | [|"$"; "ls"|] -> (pwd, sizes)
            | [|"dir"; _|] -> (pwd, sizes)
            | [|sizeStr; _|] ->
                let size = Int32.Parse(sizeStr)
                (pwd, updateSizes pwd size sizes)
            | _ -> failwith "Failed to parse"
            
        let (_, sizes) =
            lines
            |> List.fold handleLine ([], Map.empty)
            
        sizes
            
    [<AocSolver(2022, 7, Level = 1)>]
    let solve1 (input: string list) =
        input
        |> calculateSizes
        |> Map.values
        |> Seq.filter (fun sz -> sz <= 100000)
        |> Seq.sum
        
    [<AocSolver(2022, 7, Level = 2)>]
    let solve2 (input: string list) =
        let sizes = calculateSizes input
        let availableSpace = 70000000 - sizes[["/"]]
        let spaceNeeded = 30000000 - availableSpace
        
        sizes
        |> Map.values
        |> Seq.filter (fun sz -> spaceNeeded <= sz)
        |> Seq.min
