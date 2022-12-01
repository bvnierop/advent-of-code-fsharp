namespace AdventOfCode.Lib

module Solver = 
    open System.Reflection
    
    type Solver = {
        Year: int;
        Day: int
        Level: int
        Method: MethodInfo
    }

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
        
    let runSolver input solver =
        printfn $"Running solver for {solver.Year}-12-{solver.Day:D2}, level {solver.Level}."
        
        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
        let result = solver.Method.Invoke(null, [|input|])
        sw.Stop()
        
        printfn $"{result}"
        printfn $"Solver ran in {sw.Elapsed}.{System.Environment.NewLine}"
        
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
                
            let input = System.IO.File.ReadAllLines(inFile) |> Array.toList
            Array.iter (runSolver input) daySolvers
