module AdventOfCode.Solutions._2022.Day19

open System.Runtime.InteropServices
open AdventOfCode.Lib.Solver
open System
open FParsec

type Blueprint =
    { Id: int
      OreRobotCost: int
      ClayRobotCost: int
      ObsidianRobotCost: (int * int)
      GeodeRobotCost: (int * int) }

let makeBlueprint id oreRobotOre clayRobotOre (obsidianRobotOre, obsidianRobotClay) (geodeRobotOre, geodeRobotObsidian) =
    { Id = id
      OreRobotCost = oreRobotOre
      ClayRobotCost = clayRobotOre
      ObsidianRobotCost = (obsidianRobotOre, obsidianRobotClay)
      GeodeRobotCost = (geodeRobotOre, geodeRobotObsidian) }

let sstr = skipString
let pBlueprintId = sstr "Blueprint " >>. pint32 .>> sstr ": "
let pOreRobot = sstr "Each ore robot costs " >>. pint32 .>> sstr " ore. "
let pClayRobot = sstr "Each clay robot costs " >>. pint32 .>> sstr " ore. "

let pObsidianRobot =
    sstr "Each obsidian robot costs " >>. pint32 .>> sstr " ore and " .>>. pint32
    .>> sstr " clay. "

let ppGeodeRobot =
    sstr "Each geode robot costs " >>. pint32 .>> sstr " ore and " .>>. pint32
    .>> sstr " obsidian."

let pBlueprint =
    pipe5 pBlueprintId pOreRobot pClayRobot pObsidianRobot ppGeodeRobot makeBlueprint

let pLine = parseOrDie pBlueprint

type RobotCounts =
    { OreRobots: int
      ClayRobots: int
      ObsidianRobots: int
      GeodeRobots: int }
let initialRobotCounts = { OreRobots = 1; ClayRobots = 0; ObsidianRobots = 0; GeodeRobots = 0 }

type Inventory =
    { Ore: int
      Clay: int
      Obsidian: int
      Geodes: int }
let emptyInventory = { Ore = 0; Clay = 0; Obsidian = 0; Geodes = 0 }

let maxOre blueprint =
    max blueprint.OreRobotCost blueprint.ClayRobotCost
    |> max (blueprint.ObsidianRobotCost |> fst)
    |> max (blueprint.GeodeRobotCost |> fst)
    
let maxClay blueprint = blueprint.ObsidianRobotCost |> snd
let maxObsidian blueprint = blueprint.GeodeRobotCost |> snd

let spawnOreRobot blueprint robots inventory =
    if blueprint.OreRobotCost <= inventory.Ore then
         Some ({ robots with OreRobots = robots.OreRobots + 1 },
         { inventory with Ore = inventory.Ore - blueprint.OreRobotCost })
    else None

let spawnClayRobot blueprint robots inventory =
    if blueprint.ClayRobotCost <= inventory.Ore then
         Some ({ robots with ClayRobots = robots.ClayRobots + 1 },
         { inventory with Ore = inventory.Ore - blueprint.ClayRobotCost })
    else None
        

let spawnObsidianRobot blueprint robots inventory =
    let (oreCost, clayCost) = blueprint.ObsidianRobotCost

    if oreCost <= inventory.Ore && clayCost <= inventory.Clay then
        Some (
         { robots with ObsidianRobots = robots.ObsidianRobots + 1 },
         { inventory with
             Ore = inventory.Ore - oreCost
             Clay = inventory.Clay - clayCost })
    else None

let spawnGeodeRobot blueprint robots inventory =
    let (oreCost, obsidianCost) = blueprint.GeodeRobotCost

    if oreCost <= inventory.Ore && obsidianCost <= inventory.Obsidian then
        Some (
         { robots with GeodeRobots = robots.GeodeRobots + 1 },
         { inventory with
             Ore = inventory.Ore - oreCost
             Obsidian = inventory.Obsidian - obsidianCost })
    else None

let updateInventory robotCounts inventory =
    { inventory with
        Ore = inventory.Ore + robotCounts.OreRobots
        Clay = inventory.Clay + robotCounts.ClayRobots
        Obsidian = inventory.Obsidian + robotCounts.ObsidianRobots
        Geodes = inventory.Geodes + robotCounts.GeodeRobots }
                               
let simulate blueprint minutes =
    let generateChoices robots = [
        if robots.OreRobots < maxOre blueprint then spawnOreRobot
        if robots.ClayRobots < maxClay blueprint then spawnClayRobot
        if robots.ClayRobots > 0 && robots.ObsidianRobots < maxObsidian blueprint then spawnObsidianRobot
        if robots.ObsidianRobots > 0 then spawnGeodeRobot
    ]
    
    let bestPossible t inventory robots =
        let remainingMinutes = minutes - t - 1
        let maxExtraGeodes = (remainingMinutes * (remainingMinutes + 1)) / 2
        (remainingMinutes + 1) * robots.GeodeRobots + maxExtraGeodes + inventory.Geodes
    
    let rec loop t inventory robots nextRobot bestSoFar =
        if t = minutes then max bestSoFar inventory.Geodes
        else
            if bestPossible t inventory robots < bestSoFar then bestSoFar
            else
                match nextRobot with
                | None ->
                    generateChoices robots
                    |> List.scan (fun bestSoFar spawner -> loop t inventory robots (Some spawner) bestSoFar) bestSoFar
                    |> List.max
                | Some spawner ->
                    match spawner blueprint robots inventory with
                    | None -> loop <| t + 1 <| updateInventory robots inventory <| robots <| Some spawner <| bestSoFar
                    | Some (robotsIncludingNew, inventory) -> loop <| t + 1 <| updateInventory robots inventory <| robotsIncludingNew <| None <| bestSoFar
    loop 0 emptyInventory initialRobotCounts None -1
    
[<AocSolver(2022, 19, Level = 1)>]
let solve1 (input: string List) =
    input |> List.map pLine
    |> List.map (fun bp -> simulate bp 24 * bp.Id)
    |> List.sum

[<AocSolver(2022, 19, Level = 2)>]
let solve2 (input: string list) =
    input |> List.map pLine
    |> List.take (min <| List.length input <| 3)
    |> List.map (fun bp -> simulate bp 32)
    |> List.reduce (*)
