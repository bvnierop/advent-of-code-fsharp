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
        let mutable pwd = []
        let mutable sizes = Map.empty
        for line in lines do
            match line.Split(" ") with
            | [|"$"; "cd"; ".."|] -> pwd <- List.tail pwd
            | [|"$"; "cd"; dirName|] -> pwd <- dirName :: pwd
            | [|"$"; "ls"|] -> ()
            | [|"dir"; _|] -> ()
            | [|sizeStr; _|] when sizeStr <> "dir" ->
                let size = Int32.Parse(sizeStr)
                sizes <- updateSizes pwd size sizes 
            | _ -> failwith "Failed to parse"
        sizes
            
    [<AocSolver(2022, 7, Level = 1)>]
    let solve1 (input: string list) =
        let sizes = calculateSizes input
        let smallEnough = Map.filter (fun _ size -> size <= 100000) sizes 
        Map.fold (fun sum _ size -> sum + size) 0 smallEnough 
        
    [<AocSolver(2022, 7, Level = 2)>]
    let solve2 (input: string list) =
        let sizes = calculateSizes input
        let availableSpace = 70000000 - sizes[["/"]]
        let spaceNeeded = 30000000 - availableSpace;
        sizes
        |> Map.values
        |> Seq.filter (fun sz -> sz >= spaceNeeded)
        |> Seq.min
