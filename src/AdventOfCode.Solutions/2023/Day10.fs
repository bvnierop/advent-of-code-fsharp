namespace AdventOfCode.Solutions._2023

open AdventOfCode.Lib.Solver

module Day10 =
    type PipeConnection =
        | NorthSouth // Vertical
        | EastWest // Horizontal
        | NorthEast
        | NorthWest
        | SouthEast
        | SouthWest
        | Start
        | Nothing

    module PipeConnection =
        let ofChar =
            function
            | '|' -> NorthSouth
            | '-' -> EastWest
            | 'L' -> NorthEast
            | 'J' -> NorthWest
            | '7' -> SouthWest
            | 'F' -> SouthEast
            | 'S' -> Start
            | _ -> Nothing

    let parse input =
        input
        |> List.map String.toArray
        |> List.toArray
        |> array2D
        |> Array2D.map PipeConnection.ofChar

    let findStart map =
        match Array2D.findIndex (fun x -> x = PipeConnection.Start) map with
        | Some(x, y) -> (x, y)
        | None -> failwith "No start found"


    let (|ConnectsToSouth|_|) =
        function
        | PipeConnection.NorthSouth
        | PipeConnection.SouthWest
        | PipeConnection.SouthEast -> Some()
        | _ -> None

    let (|ConnectsToNorth|_|) =
        function
        | PipeConnection.NorthSouth
        | PipeConnection.NorthWest
        | PipeConnection.NorthEast -> Some()
        | _ -> None

    let (|ConnectsToEast|_|) =
        function
        | PipeConnection.EastWest
        | PipeConnection.SouthEast
        | PipeConnection.NorthEast -> Some()
        | _ -> None

    let (|ConnectsToWest|_|) =
        function
        | PipeConnection.EastWest
        | PipeConnection.SouthWest
        | PipeConnection.NorthWest -> Some()
        | _ -> None

    let determineShape map (x, y) =
        let neighbours =
            [ (x, y - 1) // North
              (x + 1, y) // East
              (x, y + 1) // South
              (x - 1, y) ] // West
            |> List.map (fun (xx, yy) -> Array2D.getDefault xx yy PipeConnection.Nothing map)

        match neighbours with
        | [ ConnectsToSouth; _; ConnectsToNorth; _ ] -> PipeConnection.NorthSouth
        | [ _; ConnectsToWest; _; ConnectsToEast ] -> PipeConnection.EastWest
        | [ ConnectsToSouth; ConnectsToWest; _; _ ] -> PipeConnection.NorthEast
        | [ ConnectsToSouth; _; _; ConnectsToEast ] -> PipeConnection.NorthWest
        | [ _; _; ConnectsToNorth; ConnectsToEast ] -> PipeConnection.SouthWest
        | [ _; ConnectsToWest; ConnectsToNorth; _ ] -> PipeConnection.SouthEast
        | _ -> failwith "Could not detect shape"

    let shapeAt (x, y) (map: PipeConnection[,]) =
        match map[y, x] with
        | Start -> determineShape map (x, y)
        | other -> other

    let next (curx, cury) prev (map: PipeConnection[,]) =
        let rec next' (curx, cury) prev shape =
            match shape with
            | NorthSouth -> [ (curx, cury - 1); (curx, cury + 1) ]
            | EastWest -> [ (curx - 1, cury); (curx + 1, cury) ]
            | NorthEast -> [ (curx + 1, cury); (curx, cury - 1) ]
            | NorthWest -> [ (curx - 1, cury); (curx, cury - 1) ]
            | SouthEast -> [ (curx + 1, cury); (curx, cury + 1) ]
            | SouthWest -> [ (curx - 1, cury); (curx, cury + 1) ]
            // | Start -> next' (curx, cury) prev (determineShape map (curx, cury))
            | _ -> failwith "Invalid shape for finding next"

        // next' (curx, cury) prev map[cury, curx]
        next' (curx, cury) prev (shapeAt (curx, cury) map)
        |> List.filter (fun (x, y) -> (x, y) <> prev)
        |> List.head

    let findLoop map =
        let start = findStart map

        let rec follow (curx, cury) prev path =
            let nextx, nexty = next (curx, cury) prev map

            match map[nexty, nextx] with
            | PipeConnection.Start -> (curx, cury) :: path
            | _ -> follow (nextx, nexty) (curx, cury) ((curx, cury) :: path)

        follow (next start start map) start [ start ]
    let findInnerArea map =
        let path = findLoop map |> Set.ofList
        let verticalPipeInLoop x y =
            let shape = shapeAt (x, y) map
            if Set.contains (x, y) path then
                match shape with
                | NorthSouth | NorthEast | NorthWest -> true
                | _ -> false
            else false

        Array2D.foldi (fun x y (cellsInLoop, inLoop) _shape ->
            let inLoop' = if x = 0 then false else inLoop
            if verticalPipeInLoop x y then (cellsInLoop, not inLoop')
            elif inLoop' && not (Set.contains (x, y) path) then (Set.add (x, y) cellsInLoop, inLoop')
            else (cellsInLoop, inLoop')) (Set.empty, false) map
        |> fst |> Set.count

    [<AocSolver(2023, 10, Level = 1)>]
    let solve1 (input: string list) =
        findLoop (parse input) |> List.length |> (fun x -> x / 2 + 1)

    [<AocSolver(2023, 10, Level = 2)>]
    let solve2 (input: string list) =
        findInnerArea (parse input)
