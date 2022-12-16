namespace AdventOfCode.Solutions._2022

open AdventOfCode.Lib.Solver
open FParsec
open System

module Day16 =
    let pTag = anyString 2
    let pLine =
        skipString "Valve " >>. pTag .>> skipString " has flow rate="
        .>>. (pint32 .>> (skipString "; tunnels lead to valves " <|> skipString "; tunnel leads to valve ")
              .>>. sepBy pTag (pstring ", "))
        
    let parseInput (input: string list) =
        input |> List.map (parseOrDie pLine)
        |> List.fold (fun map (tag, data) -> Map.add tag data map) Map.empty
        
    let makeLookup adj =
        adj |> Map.keys |> Seq.indexed
        |> Seq.fold (fun lookup (i, tag) -> Map.add tag i lookup) Map.empty
        
    let floydWarshall adj lookup =
        let maxValue = Map.values lookup |> Seq.max
        let dist = Array2D.init (maxValue + 1) (maxValue + 1) (fun s d -> 100 * 100)
        // Set v -> v to 0
        for v = 0 to maxValue do dist[v,v] <- 0
        
        // Set known distances
        adj |> Map.iter (fun s (_, n) ->
            for d in n do
                dist[lookup[s],lookup[d]] <- 1)
        
        // Run FW
        for k = 0 to maxValue do
            for i = 0 to maxValue do
                for j = 0 to maxValue do
                    dist[i,j] <- min dist[i,j]
                                     (dist[i,k] + dist[k,j])
        dist
        
    let makePressureLookup adj (lookup: Map<string, int>) =
        adj |> Map.fold (fun state valve (pressure, _) ->
            Map.add lookup[valve] pressure state) Map.empty
        
    let dfs from distances (pressures: Map<int, int>) =
        let rec loop t at opened pressure =
            let mutable bestScore = pressure
            for d = 0 to (Array2D.length1 distances) - 1 do
                let targetTime = t - distances[at,d] - 1
                if not <| Set.contains d opened && pressures[d] > 0 && targetTime >= 0 then
                    bestScore <- max bestScore
                                     (loop <| t - distances[at,d] - 1 <| d <| Set.add d opened <| pressure + (targetTime * pressures[d]))
            bestScore
        loop 30 from Set.empty 0
        
    let dfs2 from distances (pressures: Map<int, int>) =
        let mutable dp = Map.empty<int64, int>
        
        let updateDp (bitset: int64) (pressure: int) =
            dp <- Map.change bitset (function | None -> Some pressure | Some e -> Some (max e pressure)) dp
        
        let openedToBitSet opened =
            opened |> Seq.fold (fun bitSet i -> bitSet ||| (1L <<< i)) 0L
            
        let rec loop t at opened pressure =
            updateDp (openedToBitSet opened) pressure
            
            let mutable bestScore = pressure
            for d = 0 to (Array2D.length1 distances) - 1 do
                let targetTime = t - distances[at,d] - 1
                if not <| Set.contains d opened && pressures[d] > 0 && targetTime >= 0 then
                    bestScore <- max bestScore
                                     (loop <| t - distances[at,d] - 1 <| d <| Set.add d opened <| pressure + (targetTime * pressures[d]))
            bestScore
        loop 26 from Set.empty 0 |> ignore
        
        let mask = Map.keys pressures |> Seq.fold (fun mask i -> if pressures[i] > 0 then mask ||| (1L <<< i) else mask) 0L
        let mutable best = 0
        for key in Map.keys dp do
            let inverse = key ^^^ mask
            if Map.containsKey inverse dp then
                best <- max best (dp[key] + dp[inverse])
        best
        
    [<AocSolver(2022, 16, Level = 1)>]
    let solve1 (input: string list) =
        let adj = input |> parseInput
        let lookup = makeLookup adj
        let distances = floydWarshall adj lookup
        let pressureLookup = makePressureLookup adj lookup
        dfs lookup["AA"] distances pressureLookup
        
    [<AocSolver(2022, 16, Level = 2)>]
    let solve2 (input: string list) =
        let adj = input |> parseInput
        let lookup = makeLookup adj
        let distances = floydWarshall adj lookup
        let pressureLookup = makePressureLookup adj lookup
        dfs2 lookup["AA"] distances pressureLookup