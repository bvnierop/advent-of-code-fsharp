namespace AdventOfCode.Solutions._2023

open AdventOfCode.Lib.Solver

module Day10 =
    type PipeConnection =
        | North
        | South
        | East
        | West
        | Start

    module PipeConnection =
        let ofChar =
            function
            | '|' -> [ North; South ]
            | '-' -> [ East; West ]
            | 'L' -> [ North; East ]
            | 'J' -> [ North; West ]
            | '7' -> [ South; West ]
            | 'F' -> [ South; East ]
            | 'S' -> [ Start ]
            | _ -> []

    let parse input =
        input
        |> List.map String.toArray
        |> List.toArray
        |> array2D
        |> Array2D.map PipeConnection.ofChar

    let findStart map =
        match Array2D.findIndex (fun x -> x = [ Start ]) map with
        | Some(x, y) -> (x, y)
        | None -> failwith "No start found"

    let (|ConnectsToSouth|_|) list =
        if List.contains South list then Some() else None

    let (|ConnectsToNorth|_|) list =
        if List.contains North list then Some() else None

    let (|ConnectsToEast|_|) list =
        if List.contains East list then Some() else None

    let (|ConnectsToWest|_|) list =
        if List.contains West list then Some() else None

    let determineShape map (x, y) =
        let neighbours =
            [ (x, y - 1) // North
              (x + 1, y) // East
              (x, y + 1) // South
              (x - 1, y) ] // West
            |> List.map (fun (xx, yy) -> Array2D.getDefault xx yy [] map)

        match neighbours with
        | [ ConnectsToSouth; _; ConnectsToNorth; _ ] -> [ North; South ]
        | [ _; ConnectsToWest; _; ConnectsToEast ] -> [ East; West ]
        | [ ConnectsToSouth; ConnectsToWest; _; _ ] -> [ North; East ]
        | [ ConnectsToSouth; _; _; ConnectsToEast ] -> [ North; West ]
        | [ _; _; ConnectsToNorth; ConnectsToEast ] -> [ South; East ]
        | [ _; ConnectsToWest; ConnectsToNorth; _ ] -> [ South; West ]
        | _ -> failwith "Could not detect shape"

    let shapeAt (x, y) (map: PipeConnection list[,]) =
        match map[y, x] with
        | [ Start ] -> determineShape map (x, y)
        | other -> other

    let offset (curx, cury) =
        function
        | North -> (curx + 0, cury - 1)
        | South -> (curx + 0, cury + 1)
        | East ->  (curx + 1, cury + 0)
        | West ->  (curx - 1, cury + 0)
        | Start -> failwith "todo"

    let next (curx, cury) prev (map: PipeConnection list[,]) =
        shapeAt (curx, cury) map
        |> List.map (offset (curx, cury))
        |> List.filter (fun (x, y) -> (x, y) <> prev)
        |> List.head

    let findLoop map =
        let start = findStart map

        let rec follow (curx, cury) prev path =
            let nextx, nexty = next (curx, cury) prev map

            match map[nexty, nextx] with
            | [ Start ] -> (curx, cury) :: path
            | _ -> follow (nextx, nexty) (curx, cury) ((curx, cury) :: path)

        follow (next start start map) start [ start ]

    let findInnerArea map =
        let path = findLoop map |> Set.ofList

        let verticalPipeInLoop x y =
            let shape = shapeAt (x, y) map

            if Set.contains (x, y) path then
                match shape with
                | ConnectsToNorth -> true
                | _ -> false
            else
                false

        Array2D.foldi
            (fun (cellsInLoop, inLoop) y x _shape ->
                let inLoop' = if x = 0 then false else inLoop

                if verticalPipeInLoop x y then
                    (cellsInLoop, not inLoop')
                elif inLoop' && not (Set.contains (x, y) path) then
                    (Set.add (x, y) cellsInLoop, inLoop')
                else
                    (cellsInLoop, inLoop'))
            (Set.empty, false)
            map
        |> fst
        |> Set.count

    [<AocSolver(2023, 10, Level = 1)>]
    let solve1 (input: string list) =
        findLoop (parse input) |> List.length |> (fun x -> x / 2)

    [<AocSolver(2023, 10, Level = 2)>]
    let solve2 (input: string list) = findInnerArea (parse input)
