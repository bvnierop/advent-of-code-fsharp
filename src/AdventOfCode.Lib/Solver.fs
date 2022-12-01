namespace AdventOfCode.Lib

open System

module Solver = 
    open System.Reflection
    
    type Solver = {
        Year: int;
        Day: int
        Level: int
        Method: MethodInfo
    }
    
    type ArgumentType =
        | StringArgument
        | StringListArgument
        | UnsupportedArgument of Type
        
    let argumentType solver =
        let args = solver.Method.GetParameters()
        let firstArg = args.[0]
        let paramType = firstArg.ParameterType
        if paramType = typeof<string> then StringArgument
        elif paramType = typeof<string list> then StringListArgument
        else UnsupportedArgument(paramType)

    type AocSolverAttribute(year: int, day: int) =
        inherit System.Attribute()
        member val Year: int = year with get
        member val Day: int = day with get
        member val Level: int = 0 with get, set
        
    let methodInfoToSolver (mi: MethodInfo) =
        let attr = mi.GetCustomAttribute(typeof<AocSolverAttribute>, false) :?> AocSolverAttribute
        { Year = attr.Year; Day = attr.Day; Level = attr.Level; Method = mi }

    let findSolvers t =
        let types = Assembly.GetAssembly(t).GetTypes()
        // let types = Assembly.GetExecutingAssembly().GetTypes()
        let aocSolvers =
            types
            |> Array.collect (fun typ -> typ.GetMethods())
            |> Array.choose (fun mi ->
                mi.CustomAttributes
                |> Seq.tryFind (fun attr -> attr.AttributeType = typeof<AocSolverAttribute>)
                |> Option.map (fun _attr -> mi))
            |> Array.map methodInfoToSolver
        aocSolvers
        
    let convertInput (input: string) solver =
        match argumentType solver with
        | UnsupportedArgument t -> failwith $"Unsupported argument type for solver: {t}"
        | StringArgument -> input.Replace("\r\n", "\n") :> obj
        | StringListArgument -> input.Replace("\r\n", "\n").Split("\n") |> Array.toList :> obj
        
    let expectedResultOpt outFileName =
        try Some(System.IO.File.ReadAllText(outFileName))
        with | _ -> None
        
    let reportResultStatus (result: string) (expectedOpt: string option) =
        expectedOpt
        |> Option.iter (fun expected ->
              if result = expected then printfn "PASSED"
              else printfn $"FAILED! Expected `${expected}`, but got `{result}`.")
        
    let runSolver (rawInput: string) outFileName solver =
        printfn $"Running solver for {solver.Year}-12-{solver.Day:D2}, level {solver.Level}."
        
        let processedInput = convertInput rawInput solver
        
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
        let result = solver.Method.Invoke(null, [|processedInput|])
        sw.Stop()
        
        printfn $"{result}"
        reportResultStatus $"{result}" (expectedResultOpt outFileName)
        printfn $"Solver ran in {sw.Elapsed}.{System.Environment.NewLine}"
        
    let outFileName inFileOpt (solver: Solver) =
        match inFileOpt with
        | None -> $"input/{solver.Year}/{solver.Day:D2}-{solver.Level}.out"
        | Some customPart -> $"input/{solver.Year}/{solver.Day:D2}-{customPart}-{solver.Level}.out"
        
    let runSolvers t year day inFileOpt =
        let daySolvers = findSolvers t
                         |> Array.filter (fun solver -> solver.Year = year && solver.Day = day)
                         |> Array.sortBy (fun solver -> solver.Level)
                         
        if Array.isEmpty daySolvers then
            printfn $"No solvers found for {year}-12-{day:D2}."
        else
            let inFile = 
                match inFileOpt with
                | None -> $"input/{year}/{day:D2}.in"
                | Some f -> $"input/{year}/{day:D2}-{f}.in"
                
            let input = System.IO.File.ReadAllText(inFile)
            Array.iter (fun s -> runSolver input (outFileName inFileOpt s) s) daySolvers
