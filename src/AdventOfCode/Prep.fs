module AdventOfCode.Prep

open AdventOfCode.Client
open Refit

let sourceFileTemplate year day =
    @$"
namespace AdventOfCode.Solutions._{year}

open AdventOfCode.Lib.Solver
open System

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
        let openingLine = "    <ItemGroup> <!-- Solutions -->"
        let closingLine = "    </ItemGroup> <!-- Solutions -->"
        
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
    
    // Create test infile and outfiles
    writeFile $"{inFilePfx}-test.in" ""
    writeFile $"{inFilePfx}-test-1.out" ""
    writeFile $"{inFilePfx}-test-2.out" ""
    
    // Create file template
    let srcFile = $"src/AdventOfCode.Solutions/{year}/Day{day:D2}.fs"
    writeFile srcFile (sourceFileTemplate year day)
    
    // Add to project?
    addFileToProject $"{year}\\Day{day:D2}.fs" "src/AdventOfCode.Solutions/AdventOfCode.Solutions.fsproj"
    ()
