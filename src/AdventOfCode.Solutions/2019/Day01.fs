namespace AdventOfCode.Solutions._2019

open AdventOfCode.Lib.Solver
open System

module Day01 =
    type Module = { Mass: int }
    type Fuel = { Mass: int }

    let inline private (|HasMass|) x = (^x : (member Mass: int) x)

    let inline fuel (HasMass mass) : Fuel =
        { Mass = mass / 3 - 2 }

    let fuelR (cargo: Module) : Fuel =
        { Mass = Seq.repeat fuel (fuel cargo)
        |> Seq.takeWhile (fun c -> 0 < c.Mass)
        |> Seq.sumBy (fun c -> c.Mass) }

    let parseModule str : Module = { Mass = Int32.parse str }

    [<AocSolver(2019, 1, Level = 1)>]
    let solve1 (input: string list) =
        input
        |> List.map (parseModule >> fuel)
        |> List.sumBy (fun f -> f.Mass)

    [<AocSolver(2019, 1, Level = 2)>]
    let solve2 (input: string list) =
        input
        |> List.map (parseModule >> fuelR)
        |> List.sumBy (fun f -> f.Mass)
