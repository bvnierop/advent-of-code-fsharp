module AdventOfCode.Solutions._2023.Day25

open System
open System.Runtime.InteropServices
open AdventOfCode.Lib.Solver
open FParsec

let pAsciiString = many1Chars asciiLetter
let pWiring = pAsciiString .>> pchar ':' .>> spaces .>>. sepBy pAsciiString spaces1
let parseLine = parseOrDie pWiring

let rec findConnectedComponents q (adj: Map<string, string list>) exceptedEdges (vis: Set<string>) res =
    match q with
    | [] -> res
    | c :: cs ->
        let neighbours =
            adj[c]
            |> List.filter (fun n -> not (Set.contains n vis))
            |> List.filter (fun n -> not (Set.contains (c, n) exceptedEdges) && not (Set.contains (n, c) exceptedEdges))
        let vis = neighbours |> List.fold (fun vis n -> Set.add n vis) vis
        let q = neighbours @ cs
        findConnectedComponents q adj exceptedEdges vis (c :: res)

let identifyConnectedGroups components adjacencyMap exceptedEdges =
    List.fold (fun (vis, groups) c ->
        if not (Set.contains c vis) then
            let connectedComponents = findConnectedComponents [c] adjacencyMap exceptedEdges (Set.add c vis) []
            let vis = List.fold (fun vis c -> Set.add c vis) vis connectedComponents
            (vis, connectedComponents :: groups)
        else (vis, groups)) (Set.empty, []) components
    |> snd

[<AocSolver(2023, 25, Level = 1)>]
let solve1 (input: string list) =
    let adjacencyList = List.map parseLine input
    let addToMap src dest map =
        Map.change src (fun v -> Some (match v with | Some v -> dest :: v | None -> [dest])) map
    let adjacencyMap =
        List.fold (fun map (source, destinations) ->
            List.fold (fun map destination ->
                map |> addToMap source destination |> addToMap destination source) map destinations) Map.empty adjacencyList

    let componentCount = Seq.length (Map.keys adjacencyMap)
    // Idea: Find connected components. Step one should find all of them. Let's verify
    let components = adjacencyMap |> Map.keys |> Seq.toList

    // let i = 0;
    // for c in components do
    //     if not (seen c) then
    //         let set = findConnectedComponents c components seen adjacencyMap []
    //         Map.add i++ set map


    // List.length groups, List.length (List.head groups), componentCount
    let edges = Map.fold (fun edges src dests ->
        List.fold (fun edges dest ->
            if src < dest then (src, dest) :: edges // ensure we only add each edge once
            else edges) edges dests) [] adjacencyMap

    let solveFor exceptedEdges components adjacencyMap =
        let groups = identifyConnectedGroups components adjacencyMap exceptedEdges
        if List.length groups = 2 then
            groups |> List.map List.length |> List.reduce (*)
        else 0

    let rec search n edges except best =
        if n = 3 then
            max best <| solveFor except components adjacencyMap
        else
            match edges with
            | [] -> best
            | e :: es ->
                max
                    (search (n + 1) es (Set.add e except) best)
                    (search n es except best)

    //let slow = search 0 edges Set.empty 0

    // randomize: Grab 1000 random node combinations, find a path, count all edges used on each path
    // Cut the three most used edges and go for it.

    let componentsArray = components |> Array.ofList
    let random = Random(10)
    let pairs =
        [0..1000]
        |> List.fold (fun selection _ -> (componentsArray[random.Next(componentsArray.Length)], componentsArray[random.Next(componentsArray.Length)]) :: selection) []

    let findPath src dst =
        let rec find Q S =
            if Queue.isEmpty Q then failwith "No path found"
            else
                let c, path = Queue.front Q
                let Q = Queue.dequeue Q
                if c = dst then List.pairs path |> Seq.toList
                else
                    let neighbours = adjacencyMap[c]
                    let neighbours = List.filter (fun n -> not (Set.contains n S)) neighbours
                    let S = List.fold (fun S n -> Set.add n S) S neighbours
                    let Q = List.fold (fun Q n -> Queue.enqueue (n, n :: path) Q) Q neighbours
                    find Q S
        find (Queue.singleton (src, [])) (Set.singleton src)

    let usedEdges = List.map (fun (src, dst) -> findPath src dst) pairs |> List.collect id
    let counts = List.countBy id usedEdges
    let sorted = counts |> List.sortByDescending snd
    let top3 = sorted |> List.take 3 |> List.map fst |> Set.ofList


    search 0 (sorted |> List.map fst |> List.take (min (List.length sorted) 25)) Set.empty 0
    // top3
    // let groups = identifyConnectedGroups components adjacencyMap top3
    // if List.length groups = 2 then
    //     groups |> List.map List.length |> List.reduce (*)
    // else -1








[<AocSolver(2023, 25, Level = 2)>]
let solve2 (input: string list) =
    2
