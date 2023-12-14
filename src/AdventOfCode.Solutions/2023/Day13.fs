module AdventOfCode.Solutions._2023.Day13

open AdventOfCode.Lib.Solver
open FParsec

let pEol = skipNewline <|> eof
let pRow = many1CharsTill anyChar pEol
let pPattern = many1Till pRow pEol
let pPatterns = many1Till pPattern pEol

let parse input =
    parseOrDie pPatterns input
    |> List.map array2D

module Array =
    let countDifferences a b =
        Array.fold2 (fun acc a b -> if a = b then acc else acc + 1) 0 a b

    let findFirstDifference a b =
        Array.tryFindIndex (fun (a, b) -> a <> b) (Array.zip a b)

module Pattern =
    let rowSlice (pattern: 'a array2d) n =
        pattern[n, *]

    let columnSlice (pattern: 'a array2d) n =
        pattern[*, n]

    type 'a Slicer = { SliceFn: 'a array2d -> int -> 'a array
                       LenFn: 'a array2d -> int
                       IndexFn: int -> int -> int * int }

    let rowSlicer = { SliceFn = rowSlice; LenFn = Array2D.length1; IndexFn = fun i j -> i, j }
    let columnSlicer = { SliceFn = columnSlice; LenFn = Array2D.length2; IndexFn = fun i j -> j, i }

    let isMirrorSlice (pattern: char array2d) (slicer: char Slicer) index  =
        let lenA = index + 1
        let lenB = slicer.LenFn pattern - index - 1
        let len = min lenA lenB
        let before = [ index .. -1 .. index - len + 1 ]
        let after = [ index + 1 .. +1 .. index + len ]

        if len = 0 then false
        else
            List.forall2 (fun b a ->
                slicer.SliceFn pattern a = slicer.SliceFn pattern b) before after

    let findMirrorSlice slicer (pattern: char array2d) =
        let len = slicer.LenFn pattern
        let indices = [ 0 .. len - 1 ]
        List.tryFindIndex (isMirrorSlice pattern slicer) indices

    let findMirrorSlices slicer (pattern: char array2d) =
        let len = slicer.LenFn pattern
        let indices = [ 0 .. len - 1 ]
        List.choose (fun i -> if isMirrorSlice pattern slicer i then Some i else None) indices

    let asString (pattern: char array2d) =
        [0..(Array2D.length1 pattern) - 1]
        |> List.map (fun i -> pattern[i, *])
        |> List.map System.String
        |> String.concat "\n"

    let mirrorScore (pattern: char array2d) =
        match findMirrorSlice rowSlicer pattern, findMirrorSlice columnSlicer pattern with
        | None, None -> failwith $"no mirrors for pattern:\n\n{asString pattern}"
        | Some _row, Some _column -> failwith "multiple mirrors"
        | Some row, None -> (row + 1) * 100
        | None, Some column -> column + 1

    let mirrorScores (pattern: char array2d) =
        let rowScores =
            findMirrorSlices rowSlicer pattern
            |> List.map (fun i -> (i + 1) * 100)

        let columnScores =
            findMirrorSlices columnSlicer pattern
            |> List.map ((+) 1)

        rowScores @ columnScores

    let newReflectionScore (pattern: char array2d) =
        let swap i1 i2 (arr: char array2d) =
            match arr[i1,i2] with
            | '.' -> arr[i1,i2] <- '#'
            | '#' -> arr[i1,i2] <- '.'
            | _ -> failwith "invalid char"

        let originalScore = mirrorScore pattern

        let result =
            Array2D.foldiWhile (fun _acc row col _c ->
                swap row col pattern
                let scores = mirrorScores pattern |> List.reject ((=) originalScore)
                swap row col pattern
                match scores with
                | [] -> true, 0
                | [score] -> false, score
                | _ -> failwith "multiple scores") 0 pattern

        if result = 0 then failwith $"no new reflections for pattern:\n\n{asString pattern}"
        else result

[<AocSolver(2023, 13, Level = 1)>]
let solve1 (input: string) =
    parse input
    |> List.sumBy Pattern.mirrorScore

[<AocSolver(2023, 13, Level = 2)>]
let solve2 (input: string) =
    parse input
    |> List.sumBy Pattern.newReflectionScore
