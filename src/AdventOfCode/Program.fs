open System
open System.ComponentModel
open AdventOfCode
open AdventOfCode.Client
open Refit

let run year day =
    AdventOfCode.Solutions.Solver.runSolvers year day
    
let sourceFileTemplate year day =
    @$"
namespace AdventOfCode.Solutions._{year}

open AdventOfCode.Solutions.Solver

module Day{day:D2} =
    [<AocSolver({year}, {day}, Level = 1)>]
    let solve1 (input: string list) =
        1
        
    [<AocSolver({year}, {day}, Level = 2)>]
    let solve2 (input: string list) =
        2
    ".Trim()

let loadSession () =
    System.IO.File.ReadAllText(".session")

let ensureDir dir =
    printfn $"Ensure directory {dir} exists."
    System.IO.Directory.CreateDirectory(dir) |> ignore
    
let overwriteFile file content =
    printfn $"Creating file {file}"
    System.IO.File.WriteAllText(file, content)
    
let writeFile file content =
    if System.IO.File.Exists(file) then
        printfn $"Skipping file {file}. It already exists."
    else
        overwriteFile file content
    
let addFileToProject path prjFile =
    // Load project file
    let lines = System.IO.File.ReadAllLines(prjFile)
    let pathLine = $"        <Compile Include=\"{path}\" />"
    
    if Array.contains pathLine lines then
        printfn $"{path} already part of project {prjFile}"
    else
        printfn $"Add {path} to project {prjFile}"
        let openingLine = "    <ItemGroup>"
        let closingLine = "    </ItemGroup>"
        
        // get the LAST item group
        let (preamble, rest) = Array.splitAt (Array.findIndexBack ((=) openingLine) lines) lines
        let (middle, postamble) = Array.splitAt (Array.findIndexBack ((=) closingLine) rest) rest
        
        let compiles = Array.sort (Array.append (Array.skip 1 middle) [|pathLine|])
        
        let chainAppend two one = Array.append one two
        
        let newLines = preamble
                       |> chainAppend [|openingLine|]
                       |> chainAppend compiles
                       |> chainAppend postamble
                       
        // REST contains openingLine, postamble contains closingLine
        
        System.IO.File.WriteAllLines(prjFile, newLines)

let prep year day =
    // Create directories
    let subFolder = $"{year}"
    ensureDir $"input/{subFolder}"
    ensureDir $"src/AdventOfCode.Solutions/{subFolder}"
    
    // Download infile
    let aocClient = RestService.For<IAdventOfCodeClient>("https://adventofcode.com")
    let session = loadSession ()
    let input = aocClient.GetInput(year, day, session)
    let inFilePfx = $"input/{year}/{day:D2}"
    overwriteFile $"{inFilePfx}.in" input
    
    // Create test infile
    writeFile $"{inFilePfx}-test.in" ""
    
    // Create file template
    let srcFile = $"src/AdventOfCode.Solutions/{year}/Day{day:D2}.fs"
    writeFile srcFile (sourceFileTemplate year day)
    
    // Add to project?
    addFileToProject $"{year}\\Day{day:D2}.fs" "src/AdventOfCode.Solutions/AdventOfCode.Solutions.fsproj"
    ()
    
module Commands =
    open System.CommandLine
    let makeRoot () = new RootCommand()
    
    let invoke (args: string array) (rootCommand: RootCommand) =
        rootCommand.Invoke(args)
    
    let makeCommand name description = new Command(name, description)
    
    let addCommand command (root: RootCommand) =
        root.AddCommand(command)
        root
    
    let makeArg name description =
        new Argument<'a>(name = name, description = description)
        
    let makeOptionalArg name description (getDefault: unit -> 'a) =
        new Argument<'a>(name, getDefault, description)
        
    let addArg arg (command: Command) =
        command.AddArgument(arg)
        command
        
    let addNewArg name description command = addArg (makeArg name description) command
        
    let addOptionalArg name description getDefault command = addArg (makeOptionalArg name description getDefault) command
    
    let setHandler2 (handler: 'a -> 'b -> unit) arg1 arg2 (command: Command) =
        command.SetHandler(handler, arg1, arg2)
        command
        
        
        
open System.CommandLine
                                        
let buildProgram =
    let yearArg = Commands.makeOptionalArg "year" "The year for which to run the command. Defaults to the current year." (fun () -> DateTime.Today.Year)
    let dayArg = Commands.makeOptionalArg "day" "The day for which to run the command. Defaults to the current day." (fun () -> DateTime.Today.Day)
    let runCommand = Commands.makeCommand "run" "Runs solvers for the given year and day."
                     |> Commands.addArg yearArg
                     |> Commands.addArg dayArg
                     |> Commands.setHandler2 run yearArg dayArg
                     
    let prepCommand = Commands.makeCommand "prep" "Prepares input and skeleton code for the given year and day."
                      |> Commands.addArg yearArg
                      |> Commands.addArg dayArg
                      |> Commands.setHandler2 prep yearArg dayArg
    
    let rootCommand = Commands.makeRoot ()
                      |> Commands.addCommand runCommand
                      |> Commands.addCommand prepCommand
                      
    rootCommand
    
[<EntryPoint>]
let main args =
    let rootCommand = buildProgram
    Commands.invoke args rootCommand
    // match args with
    // | [|"run"|] -> run ()
    // | [|"prep"|] -> prep 2020 1
    // | _ -> printfn "No arguments given."