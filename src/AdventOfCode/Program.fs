open AdventOfCode.Client
open Refit

let run () =
    AdventOfCode.Solutions.Solver.runSolvers 2021 1
    AdventOfCode.Solutions.Solver.runSolvers 2018 2
    AdventOfCode.Solutions.Solver.runSolvers 2020 1
    
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
    
let writeFile file content =
    printfn $"Creating file {file}"
    System.IO.File.WriteAllText(file, content)
    
let addFileToProject path prjFile =
    // Load project file
    let lines = System.IO.File.ReadAllLines(prjFile)
    let pathLine = $"        <Compile Include=\"{path}\" />"
    
    if Array.contains pathLine lines then
        printfn $"{path} already part of project {prjFile}"
    else
        printfn $"Add {path} to project {prjFile}"
        let closingLine = "    </ItemGroup>"
        let newLines = Array.insertAt (Array.findIndex ((=) closingLine) lines) pathLine lines
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
    writeFile $"{inFilePfx}.in" input
    
    // Create test infile
    writeFile $"{inFilePfx}-test.in" ""
    
    // Create file template
    let srcFile = $"src/AdventOfCode.Solutions/{year}/Day{day:D2}.fs"
    writeFile srcFile (sourceFileTemplate year day)
    
    // Add to project?
    addFileToProject $"{year}\\Day{day:D2}.fs" "src/AdventOfCode.Solutions/AdventOfCode.Solutions.fsproj"
    ()
    
[<EntryPoint>]
let main args =
    match args with
    | [|"run"|] -> run ()
    | [|"prep"|] -> prep 2020 1
    | _ -> printfn "No arguments given."
    0