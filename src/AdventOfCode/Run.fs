module AdventOfCode.Run

let run year day =
    AdventOfCode.Lib.Solver.runSolvers typeof<AdventOfCode.Solutions.t> year day
