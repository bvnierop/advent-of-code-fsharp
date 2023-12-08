module AdventOfCode.Solutions._2023.Day08

open AdventOfCode.Lib
open AdventOfCode.Lib.Solver
open FParsec

type Direction =
    | Left
    | Right

module Direction =
    let ofChar =
        function
        | 'L' -> Left
        | 'R' -> Right
        | _ -> failwith "Invalid direction"

type Node = { Left: string; Right: string }

module Node =
    let create left right = { Left = left; Right = right }

    let createMap listOfNodes =
        listOfNodes |> List.fold (fun map (src, dst) -> Map.add src dst map) Map.empty

type DesertMap =
    { Directions: CircularCollection<Direction>
      Nodes: Map<string, Node> }

let pNode = anyString 3
let pSrc = pNode

let pDSt =
    pchar '(' >>. pNode .>> pstring ", " .>>. pNode .>> pchar ')' ||>> Node.create

let pLine = pSrc .>> pstring " = " .>>. pDSt
let pDirection = anyChar |>> Direction.ofChar
let pDirections = manyTill pDirection newline

let pInput =
    pDirections .>> skipNewline .>>. ((sepEndBy pLine newline) |>> Node.createMap)
    .>> (skipNewline <|> eof)

let parseLine line = parseOrDie pLine line

let parseInput string =
    let directions, nodeMap = parseOrDie pInput string

    { Directions = CircularCollection.init directions
      Nodes = nodeMap }

let step map current =
    let nextDirection = CircularCollection.item map.Directions
    match nextDirection, map.Nodes[current] with
    | Left, node -> node.Left
    | Right, node -> node.Right

let findPath start finishPred map =
    let rec findPath' at map steps =
        if finishPred at then
            steps
        else
            let nextNode = step map at
            findPath'
                nextNode
                { map with
                    Directions = CircularCollection.moveNext map.Directions }
                (steps + 1)

    findPath' start map 0

[<AocSolver(2023, 8, Level = 1)>]
let solve1 (input: string) =
    let map = parseInput input
    findPath "AAA" ((=) "ZZZ") map

[<AocSolver(2023, 8, Level = 2)>]
let solve2 (input: string) =
    let map = parseInput input
    let startNodes = Map.keys map.Nodes |> Seq.filter (fun k -> k[2] = 'A') |> Seq.toList
    startNodes
    |> List.map (fun start -> findPath start (fun node -> node[2] = 'Z') map)
    |> List.map int64
    |> List.reduce Math.lcm64
