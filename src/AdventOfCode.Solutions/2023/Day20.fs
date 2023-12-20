module AdventOfCode.Solutions._2023.Day20

open System.Collections.Immutable
open AdventOfCode.Lib.Solver
open AdventOfCode.Lib
open FParsec

type Pulse =
    | High
    | Low

type Signal = Signal of (Pulse * string)

type FlipFlopState =
    | On
    | Off

module FlipFlopState =
    let flip state =
        match state with
        | On -> Off
        | Off -> On

type ModuleState =
    | BroadcasterModuleState
    | ButtonModuleState
    | FlipFlopModuleState of FlipFlopState
    | ConjunctionModuleState of Map<string, Pulse>

module ConjunctionState =
    let setMemory id pulse memory =
        ConjunctionModuleState(Map.change id (fun _ -> Some pulse) memory)

    let allHigh state =
        match state with
        | ConjunctionModuleState memory -> Map.forall (fun _k v -> v = High) memory
        | _ -> failwith "Not implemented"

type Module =
    { Id: string
      Adjacent: string list
      State: ModuleState }

module Module =
    let makeBroadcaster id adj =
        { Id = id
          Adjacent = adj
          State = BroadcasterModuleState }

    let makeFlipFlop id adj =
        { Id = id
          Adjacent = adj
          State = FlipFlopModuleState Off }

    let makeConjunction id adj =
        { Id = id
          Adjacent = adj
          State = ConjunctionModuleState Map.empty }

    let alfaStr = many1Chars asciiLetter
    let pSep = spaces1 >>. pstring "->" >>. spaces1
    let pDestinations = sepBy alfaStr (pstring ", ")

    let pBroadcaster =
        pstring "broadcaster" .>> pstring " -> " .>>. pDestinations ||>> makeBroadcaster

    let pFlipFlop = pchar '%' >>. alfaStr .>> pSep .>>. pDestinations ||>> makeFlipFlop

    let pConjunction =
        pchar '&' >>. alfaStr .>> pSep .>>. pDestinations ||>> makeConjunction

    let pModule = pBroadcaster <|> pFlipFlop <|> pConjunction
    let parseLine = parseOrDie pModule

    let parseLines lines =
        lines
        |> List.map parseLine
        |> List.fold
            (fun mods mdule -> Map.add mdule.Id mdule mods)
            Map.empty

    let receivePulse source pulse mdule =
        match mdule.State with
        | FlipFlopModuleState state ->
            match pulse with
            | High -> [], mdule
            | Low ->
                List.map
                    (fun next ->
                        ((match state with
                          | Off -> High
                          | On -> Low),
                         next))
                    mdule.Adjacent,
                { mdule with
                    State = FlipFlopModuleState(FlipFlopState.flip state) }
        | ConjunctionModuleState memory ->
            let withUpdatedMemory =
                { mdule with
                    State = ConjunctionState.setMemory source pulse memory }

            match ConjunctionState.allHigh withUpdatedMemory.State with
            | true -> (List.map (fun next -> (Low, next)) mdule.Adjacent, withUpdatedMemory)
            | false -> (List.map (fun next -> (High, next)) mdule.Adjacent, withUpdatedMemory)
        | ButtonModuleState -> (List.map (fun next -> (Low, next)) mdule.Adjacent, mdule)
        | BroadcasterModuleState -> (List.map (fun next -> (pulse, next)) mdule.Adjacent, mdule)

    let initialize modules =
        modules
        |> Map.fold
            (fun updatedModules moduleId ``module`` ->
                ``module``.Adjacent
                |> List.fold (fun updatedModules' adjacentId ->
                    match Map.tryFind adjacentId updatedModules' with
                    | Some adjacentModule ->
                        match adjacentModule.State with
                        | ConjunctionModuleState memory -> Map.change adjacentId (fun _ -> Some { adjacentModule with State = ConjunctionState.setMemory moduleId Low memory }) updatedModules'
                        | _ -> updatedModules'
                    | None -> updatedModules') updatedModules) modules

    let run modules =
        let rec loop modules' (queue: ImmutableQueue<Pulse * string * string>) (hiCount, loCount) =
            if Queue.isEmpty queue then (hiCount, loCount, modules')
            else
                let pulse, fromId, toId = Queue.front queue
                let dequeued = Queue.dequeue queue
                match Map.tryFind toId modules' with
                | Some toModule ->
                    let signals, updatedModule = receivePulse fromId pulse toModule
                    let updatedModules = Map.change toId (fun _ -> Some updatedModule) modules'
                    let updatedQueue =
                        signals
                        |> List.map (fun (pulse, target) -> (pulse, toId, target))
                        |> (fun l -> Queue.enqueueMany l dequeued)
                    loop updatedModules updatedQueue (hiCount + (if pulse = High then 1 else 0), loCount + (if pulse = Low then 1 else 0))
                | None -> loop modules' dequeued (hiCount + (if pulse = High then 1 else 0), loCount + (if pulse = Low then 1 else 0))
        let initialQueue = Queue.empty |> Queue.enqueue (Low, "start", "broadcaster")
        loop modules initialQueue (0, 0)

    let run2 modules targetModuleId targetPulse =
        let rec loop modules' (queue: ImmutableQueue<Pulse * string * string>) conclusion =
            if Queue.isEmpty queue then conclusion, modules'
            else
                let pulse, fromId, toId = Queue.front queue
                let dequeued = Queue.dequeue queue
                let conclusion = conclusion || (fromId = targetModuleId && pulse = targetPulse)
                match Map.tryFind toId modules' with
                | Some toModule ->
                    let signals, updatedModule = receivePulse fromId pulse toModule
                    let updatedModules = Map.change toId (fun _ -> Some updatedModule) modules'
                    let updatedQueue =
                        signals
                        |> List.map (fun (pulse, target) -> (pulse, toId, target))
                        |> (fun l -> Queue.enqueueMany l dequeued)
                    loop updatedModules updatedQueue conclusion
                | None -> loop modules' dequeued conclusion
        let initialQueue = Queue.empty |> Queue.enqueue (Low, "start", "broadcaster")
        loop modules initialQueue false



[<AocSolver(2023, 20, Level = 1)>]
let solve1 (input: string list) =
    let modules = Module.parseLines input |> Module.initialize
    [1..1000]
    |> List.fold (fun (hi, lo, modules) _i ->
        let newHi, newLo, newModules = Module.run modules
        (hi + newHi, lo + newLo, newModules)) (0, 0, modules)
    |> fun (hi, lo, _) -> (hi * lo)


[<AocSolver(2023, 20, Level = 2)>]
let solve2 (input: string list) =
    let modules = Module.parseLines input |> Module.initialize
    (*
        &bg -> kz
        &sj -> kz
        &qq -> kz
        &ls -> kz
    *)
    let countSteps target modules =
        let rec loop modules steps =
            let res, modules = Module.run2 modules target High
            if res then steps + 1
            else loop modules (steps + 1)
        loop modules 0

    let a = countSteps "bg" modules |> int64
    let b = countSteps "sj" modules |> int64
    let c = countSteps "qq" modules |> int64
    let d = countSteps "ls" modules |> int64

    Math.lcm64 a (Math.lcm64 b (Math.lcm64 c d))
