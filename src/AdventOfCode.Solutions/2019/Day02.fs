namespace AdventOfCode.Solutions._2019

open AdventOfCode.Lib.Solver
open System

module Day02 =
    type IntCodeProgram = Map<int, int>

    type OpCode =
    | Add = 1
    | Multiply = 2
    | Halt = 99

    type ComputerState = Running | Halted
    type Computer = {
        Memory: IntCodeProgram
        InstructionPointer: int
        State: ComputerState
    }
    module Computer =
        let create program = { Memory = program; InstructionPointer = 0; State = Running }
        let readMemory offset computer = computer.Memory[offset]
        let readMemoryPointer offset computer = computer.Memory[computer.Memory[offset]]
        let setMemory offset value computer =
            { computer with Memory = Map.change offset (fun _ -> Some value) computer.Memory }

        let private executeOpcode opcode computer =
            let fn =
                match opcode with
                | OpCode.Add -> Some (+)
                | OpCode.Multiply -> Some (*)
                | _ -> None
            match fn with
            | None -> computer.Memory
            | Some f ->
                Map.change (readMemory (computer.InstructionPointer + 3) computer)
                    (fun _ -> Some (f (readMemoryPointer (computer.InstructionPointer + 1) computer)
                                        (readMemoryPointer (computer.InstructionPointer + 2) computer)))
                        computer.Memory

        let private calculateState = function
        | OpCode.Halt -> Halted
        | _ -> Running

        let executeNext computer =
            let opcode : OpCode = enum computer.Memory[computer.InstructionPointer]
            { computer with InstructionPointer = computer.InstructionPointer + 4;
                            Memory = executeOpcode opcode computer;
                            State = calculateState opcode }


        let rec run computer =
            match computer.State with
            | Halted -> computer
            | Running -> executeNext computer |> run


    let parse str =
        str
        |> String.splitBy ","
        |> Array.map Int32.Parse
        |> Array.indexed
        |> Map.ofArray

    [<AocSolver(2019, 2, Level = 1)>]
    let solve1 (input: string) =
        input
        |> parse
        |> Computer.create
        |> Computer.setMemory 1 12
        |> Computer.setMemory 2 2
        |> Computer.run
        |> Computer.readMemory 0

    [<AocSolver(2019, 2, Level = 2)>]
    let solve2 (input: string) =
        { 0..10000 }
        |> Seq.map (fun n ->
            let noun = n / 100
            let verb = n % 100
            input |> parse |> Computer.create
            |> Computer.setMemory 1 noun
            |> Computer.setMemory 2 verb
            |> Computer.run |> Computer.readMemory 0)
        |> Seq.indexed
        |> Seq.find (fun (_, x) -> x = 19690720)
        |> fst
