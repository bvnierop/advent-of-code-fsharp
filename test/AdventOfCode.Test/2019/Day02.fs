module AdventOfCode.Test._2019.Day02

open System
open Xunit
open Swensen.Unquote
open AdventOfCode.Solutions._2019.Day02

[<Fact>]
let ``parse an intcode program`` () =
    test <@ parse "1,2,3,1" = Map [ (0, 1); (1, 2); (2, 3); (3, 1) ] @>

let runProgramToCompletion str =
    let program = parse str
    let computer = Computer.create program
    Computer.run computer

[<Fact>]
let ``Second testcase`` () =
    test <@ runProgramToCompletion "1,0,0,0,99" |> Computer.readMemory 0 = 2 @>

[<Fact>]
let ``Third testcase`` () =
    test <@ runProgramToCompletion "2,3,0,3,99" |> Computer.readMemory 3 = 6 @>

[<Fact>]
let ``Fourth testcase`` () =
    test <@ runProgramToCompletion "2,4,4,5,99,0" |> Computer.readMemory 5 = 9801 @>

let runProgramForSteps str n =
    let program = parse str
    let computer = Computer.create program
    let rec recurse remaining computer =
        if remaining = 0 then computer
        else recurse (remaining - 1) (Computer.executeNext computer)
    recurse n computer

let testProgram =  "1,9,10,3,2,3,11,0,99,30,40,50"

[<Fact>]
let ``move ip forward when executing instruction`` () =
    let computer = runProgramForSteps testProgram 1
    test <@ computer.InstructionPointer = 4 @>

[<Fact>]
let ``Execute `add` instruction`` () =
    let computer = runProgramForSteps testProgram 1
    test <@ Computer.readMemory 3 computer = 70 @>

[<Fact>]
let ``Execute `mul` instruction`` () =
    let computer = runProgramForSteps testProgram 2
    test <@ Computer.readMemory 0 computer = 3500 @>

[<Fact>]
let ``Execute `halt` instruction`` () =
    let computer = runProgramForSteps testProgram 3
    test <@ computer.State = Halted @>

[<Fact>]
let ``Run until halted`` () =
    let computer = runProgramToCompletion testProgram
    test <@ computer.State = Halted @>
    test <@ Computer.readMemory 0 computer = 3500 @>
