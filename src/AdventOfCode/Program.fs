open System
open AdventOfCode
    
let buildProgram =
    let yearArg = Commands.makeOptionalArg "year" "The year for which to run the command. Defaults to the current year." (fun () -> DateTime.Today.Year)
    let dayArg = Commands.makeOptionalArg "day" "The day for which to run the command. Defaults to the current day." (fun () -> DateTime.Today.Day)
    
    let inFileOpt = Commands.makeOption "--file" "Specify the input file name."
    
    let runCommand = Commands.makeCommand "run" "Runs solvers for the given year and day."
                     |> Commands.addArg yearArg
                     |> Commands.addArg dayArg
                     |> Commands.addOption inFileOpt
                     |> Commands.setHandler3 Run.run yearArg dayArg inFileOpt
                     
    let prepCommand = Commands.makeCommand "prep" "Prepares input and skeleton code for the given year and day."
                      |> Commands.addArg yearArg
                      |> Commands.addArg dayArg
                      |> Commands.setHandler2 Prep.prep yearArg dayArg
    
    let rootCommand = Commands.makeRoot ()
                      |> Commands.addCommand runCommand
                      |> Commands.addCommand prepCommand
                      
    rootCommand
    
[<EntryPoint>]
let main args =
    let rootCommand = buildProgram
    Commands.invoke args rootCommand