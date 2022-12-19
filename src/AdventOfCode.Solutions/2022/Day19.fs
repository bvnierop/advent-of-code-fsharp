module AdventOfCode.Solutions._2022.Day19

open System.Runtime.InteropServices
open AdventOfCode.Lib.Solver
open System
open FParsec

// Solution direction p1
//  1. Parse input, store in a blueprint type
//  2. For each blueprint, run dfs
//  3. dfs: for each minute, make possible choices

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
    if robots.OreRobots < (maxOre blueprint) && blueprint.OreRobotCost <= inventory.Ore then
         Some ({ robots with OreRobots = robots.OreRobots + 1 },
         { inventory with Ore = inventory.Ore - blueprint.OreRobotCost })
    else None

let spawnClayRobot blueprint robots inventory =
    if robots.ClayRobots < (maxClay blueprint) && blueprint.ClayRobotCost <= inventory.Ore then
         Some ({ robots with ClayRobots = robots.ClayRobots + 1 },
         { inventory with Ore = inventory.Ore - blueprint.ClayRobotCost })
    else None
        

let spawnObsidianRobot blueprint robots inventory =
    let (oreCost, clayCost) = blueprint.ObsidianRobotCost

    if robots.ObsidianRobots < (maxObsidian blueprint) && oreCost <= inventory.Ore && clayCost <= inventory.Clay then
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
    
let canMakeAllRobots blueprint inventory =
    let (obsidianOre, obsidianClay) = blueprint.ObsidianRobotCost
    let (geodeOre, geodeObsidian) = blueprint.GeodeRobotCost
    blueprint.OreRobotCost <= inventory.Ore &&
    blueprint.ClayRobotCost <= inventory.Ore &&
    obsidianOre <= inventory.Ore &&
    geodeOre <= inventory.Ore &&
    obsidianClay <= inventory.Clay &&
    geodeObsidian <= inventory.Obsidian
                               
let random seed =
    let rnd = new Random(seed)
    (fun () -> rnd.Next(1, 101))
    
let attempt minutes blueprint random =
    let thingsToTry = [
        (spawnGeodeRobot, 100)
        (spawnObsidianRobot, 80)
        (spawnClayRobot, 50)
        (spawnOreRobot, 80)
        ((fun b r i -> Some (r, i)), 100)
    ]
    
    let rec loop t inventory robots =
        if t = minutes then inventory.Geodes
        else
            let (robotsAfterSpawning, inventoryAfterSpawning) =
                thingsToTry
                |> List.map (fun (spawner, odds) ->
                    let num = random ()
                    if num <= odds then spawner blueprint robots inventory
                    else None)
                |> List.pick id
            let inventoryAfterHarvest = updateInventory robots inventoryAfterSpawning
            loop <| t + 1 <| inventoryAfterHarvest <| robotsAfterSpawning
    loop 0 emptyInventory initialRobotCounts

let attemptOften attempts minutes blueprint =
    let randomizer = random 0
    [0..attempts]
    |> List.map (fun i -> attempt minutes blueprint randomizer)
    |> List.max

[<AocSolver(2022, 19, Level = 1)>]
let solve1 (input: string list) =
    input |> List.map pLine
    |> List.map (fun bp -> attemptOften 25000 24 bp * bp.Id)
    |> List.sum

[<AocSolver(2022, 19, Level = 2)>]
let solve2 (input: string list) =
    input |> List.map pLine
    |> List.take (min <| List.length input <| 3)
    |> List.map (attemptOften 200000 32)
    |> List.reduce (*)

