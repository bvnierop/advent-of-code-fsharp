module AdventOfCode.Run

let run year day inFile =
    let inFileOpt =
        match inFile with
        | null -> None
        | s -> Some s
    AdventOfCode.Lib.Solver.runSolvers typeof<AdventOfCode.Solutions.t> year day inFileOpt
