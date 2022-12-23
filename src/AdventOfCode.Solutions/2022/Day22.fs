namespace AdventOfCode.Solutions._2022

open AdventOfCode.Lib.Solver
open System
open FParsec

module Day22 =
    let modE a b = ((a % b) + b) % b
    
    type Instruction = Step of int | Turn of char
    type Me = { X: int; Y: int; Dir: int }
    
    type Board = {
        Grid: char array array
        TransposedGrid: char array array
        Me: Me
    }
    
    let memo f =
        let mutable cache = Array.create 500 None
        let memoized x =
            if Option.isNone cache[x] then Array.set cache x (Some (f x))
            Option.get cache[x]
        memoized
    
    let minX' y board = Array.findIndex ((=) ' ' >> not) board.Grid[y]
    let maxX' y board = Array.findIndexBack ((=) ' ' >> not) board.Grid[y]
    let minY' x board = Array.findIndex ((=) ' ' >> not) board.TransposedGrid[x]
    let maxY' x board = Array.findIndexBack ((=) ' ' >> not) board.TransposedGrid[x]
    let minX = memo minX'
    let maxX = memo maxX'
    let minY = memo minY'
    let maxY = memo maxY'
    let height board = Array.length board.Grid
    let width board = Array.length board.Grid[0]
    let get x y board =
        if x < 0 || x >= width board || y < 0 || y >= height board then ' '
        else board.Grid[y][x]
    
    let pDir = pchar 'R' <|> pchar 'L' |>> Turn
    let pStep = pint32 |>> Step
    let pInstr = pStep <|> pDir
    let pInstructions = many pInstr
    
    /// Ensures that all rows have the same length, so that we can transpose.
    let fix grid =
        let maxLen = grid |> List.maxBy Seq.length |> Seq.length
        List.map <| String.padRight maxLen <| grid
        
    let gridToArray grid = grid |> List.map Seq.toArray |> List.toArray
    let makeBoard grid =
        let tmp = { Grid = grid; TransposedGrid = Array.transpose grid; Me = { X = 0 ; Y = 0; Dir = 0 }}
        { tmp with Me = { tmp.Me with X = minX 0 tmp } }
        
    let parse input =
        match input |> List.splitOnExclusive String.isNullOrEmpty with
        | [grid; [instructions]] -> (grid |> fix |> gridToArray |> makeBoard, parseOrDie pInstructions instructions)
        | _ -> failwith "Invalid input"
        
    let dx = [|1;0;-1; 0|]
    let dy = [|0;1; 0;-1|]
    
    let wrapX x y board = 
        if x < (minX y board) then maxX y board
        else if x > (maxX y board) then minX y board
        else x
    let wrapY x y board =
        if y < minY x board then maxY x board
        else if y > maxY x board then minY x board
        else y
        
    let putMeOnBoard me board =
        let mutable x = me.X
        let mutable y = me.Y
        if me.Dir = 0 || me.Dir = 2 then
            x <- wrapX x y board
        else
            y <- wrapY x y board
        { me with X = x; Y = y }
        
    let rec doStep fixMe n board =
        if n = 0 then board
        else
            let me = board.Me
            let newMe = fixMe { me with X = me.X + dx[me.Dir]; Y = me.Y + dy[me.Dir] } board
            if board.Grid[newMe.Y][newMe.X] = '.' then
                doStep fixMe <| n - 1 <| { board with Me = newMe }
            else doStep fixMe <| n - 1 <| board
        
    let printBoard board =
        let dir n = match n with
                    | 0 -> ">"
                    | 1 -> "v"
                    | 2 -> "<"
                    | 3 -> "^"
                    | _ -> failwith "invalid direction"
                  
        for y = 0 to (Array.length board.Grid) - 1 do
            for x = 0 to (Array.length board.Grid[y]) - 1 do
                if (board.Me.X = x && board.Me.Y = y) then printf $"{dir board.Me.Dir}"
                else printf $"{board.Grid[y][x]}"
            printfn ""
        printfn "-------------------------"
        board
        
    let processInstruction fixMe board instruction =
        // printfn $"Instruction: {instruction}"
        match instruction with
        | Turn 'R' -> { board with Me = { board.Me with Dir = modE <| board.Me.Dir + 1 <| 4 }}
        | Turn 'L' -> { board with Me = { board.Me with Dir = modE <| board.Me.Dir - 1 <| 4 }}
        | Step n -> doStep fixMe n board
        | _ -> failwith $"Invalid instruction: {instruction}"
        // |> printBoard
        
    let password board =
        let me = board.Me
        1000 * (me.Y + 1) + 4 * (me.X + 1) + me.Dir
        
    [<AocSolver(2022, 22, Level = 1)>]
    let solve1 (input: string list) =
        let (board, instructions) = parse input
        instructions
        |> Seq.scan (processInstruction putMeOnBoard) board
        |> Seq.last
        |> password
        
    let faceSize board =
        board.Grid
        |> Array.sumBy (Seq.countWhere (fun c -> c <> ' '))
        |> (fun n -> n / 6)
        |> (fun n -> int <| sqrt (float n))
        
    /// Find the inner corners of the grid. Inner corners are places where three faces meet.
    /// We can find them by checking a 2x2 area, ensuring it has a single space.
    let findInnerCorners board =
        let mutable corners = []
        for y = 0 to (height board) - 2 do
            for x = 0 to (width board) - 2 do
                let spaceCount = [(x,y);(x+1,y);(x,y+1);(x+1,y+1)]
                                  |> List.countWhere (fun (xx, yy) -> board.Grid[yy][xx] = ' ')
                if spaceCount = 1 then corners <- (x,y) :: corners
        corners
        
    //  In order to stitch, for both clockwise and counter clockwise, we will look at a 2x2 area, so that we always look
    //  at the filled edge and the empty edge. We then stitch them up. We want the location to map to be the empty edge, so we need an offset.
    //    For example, if the edge we look at is ' .', then the offset is 0, but if the edge we look at is '. ' then it's +1
    //
    //  We can look at every 2x2 area as a set of 4 cells that are, or are not occupied. We can encode these in a bitset
    //  of at most 16 combinations, not all of which are possible. We can map those bitsets to both heading and offset
    //  for both clockwise and counter-clockwise directions.
    //
    //      Occupied = 1, Empty = 0. Indices like this: 01
    //                                                  23
    //  Let's describe the clockwise mapping for all possibilities. Here's what we need:
    //    - A HEADING for stitching
    //    - An OFFSET to convert the top-left corner of the 2x2 area to the cell we're looking at
    //    - The DIRECTION that we COME FROM if we reach this cell
    //    - The direction that the COUNTER CLOCKWISE has to switch to
    type Heading = | East = 0 | South = 1 | West = 2 | North = 3
    type StitchInfo = { TraverseHeading: Heading; Offset: int * int; SrcHeading: Heading; DstHeading: Heading }
    
    let stitchLookup = [|
        //  Empty space (IMPOSSIBLE)
        //  ..
        //  ..
        //  Mask: 0 + 0 + 0 + 0 =  0
        None
        //  Outer corner bottom right
        //  1.
        //  ..
        //  Mask: 1 + 0 + 0 + 0 =  1
        Some ( { TraverseHeading = Heading.West;  Offset = (0, 1); SrcHeading = Heading.South; DstHeading = Heading.North },
               { TraverseHeading = Heading.North; Offset = (1, 0); SrcHeading = Heading.East;  DstHeading = Heading.West } )
        //  Outer corner bottom left
        //  .2
        //  ..
        //  Mask: 0 + 2 + 0 + 0 =  2
        Some ( { TraverseHeading = Heading.North; Offset = (0, 0); SrcHeading = Heading.West;  DstHeading = Heading.East },
               { TraverseHeading = Heading.East;  Offset = (1, 1); SrcHeading = Heading.South; DstHeading = Heading.North } )
        //  Straight bottom
        //  12
        //  ..
        //  Mask: 1 + 2 + 0 + 0 =  3
        Some ( { TraverseHeading = Heading.West; Offset = (0, 1); SrcHeading = Heading.South; DstHeading = Heading.North },
               { TraverseHeading = Heading.East; Offset = (1, 1); SrcHeading = Heading.South; DstHeading = Heading.North } )
        //  Outer corner top right
        //  ..
        //  4.
        //  Mask: 0 + 0 + 4 + 0 =  4
        Some ( { TraverseHeading = Heading.South; Offset = (1, 1); SrcHeading = Heading.East;  DstHeading = Heading.West },
               { TraverseHeading = Heading.West;  Offset = (0, 0); SrcHeading = Heading.North; DstHeading = Heading.South } )
        //  Straight right
        //  1.
        //  4.
        //  Mask: 1 + 0 + 4 + 0 =  5
        Some ( { TraverseHeading = Heading.South; Offset = (1, 1); SrcHeading = Heading.East; DstHeading = Heading.West },
               { TraverseHeading = Heading.North; Offset = (1, 0); SrcHeading = Heading.East; DstHeading = Heading.West } )
        //  Not an edge. IMPOSSIBLE
        //  .2
        //  4.
        //  Mask: 0 + 2 + 4 + 0 =  6
        None
        //  Inner corner bottom right
        //  12
        //  4.
        //  Mask: 1 + 2 + 4 + 0 =  7
        Some ( { TraverseHeading = Heading.South; Offset = (1, 1); SrcHeading = Heading.East;  DstHeading = Heading.West },
               { TraverseHeading = Heading.East;  Offset = (1, 1); SrcHeading = Heading.South; DstHeading = Heading.North } )
        //  Outer corner top left
        //  ..
        //  .8
        //  Mask: 0 + 0 + 0 + 8 =  8
        Some ( { TraverseHeading = Heading.East;  Offset = (1, 0); SrcHeading = Heading.North; DstHeading = Heading.South },
               { TraverseHeading = Heading.South; Offset = (0, 1); SrcHeading = Heading.West;  DstHeading = Heading.East } )
        //  Not an edge. IMPOSSIBLE
        //  1.
        //  .8
        //  Mask: 1 + 0 + 0 + 8 =  9
        None
        //  Straight left
        //  .2
        //  .8
        //  Mask: 0 + 2 + 0 + 8 =  10
        Some ( { TraverseHeading = Heading.North; Offset = (0, 0); SrcHeading = Heading.West; DstHeading = Heading.East },
               { TraverseHeading = Heading.South; Offset = (0, 1); SrcHeading = Heading.West; DstHeading = Heading.East } )
        //  Inner corner bottom left
        //  12
        //  .8
        //  Mask: 1 + 2 + 0 + 8 =  11
        Some ( { TraverseHeading = Heading.West;  Offset = (0, 1); SrcHeading = Heading.South; DstHeading = Heading.North },
               { TraverseHeading = Heading.South; Offset = (0, 1); SrcHeading = Heading.West;  DstHeading = Heading.East } )
        //  Straight top
        //  ..
        //  48
        //  Mask: 0 + 0 + 4 + 8 =  12
        Some ( { TraverseHeading = Heading.East; Offset = (1, 0); SrcHeading = Heading.North; DstHeading = Heading.South },
               { TraverseHeading = Heading.West; Offset = (0, 0); SrcHeading = Heading.North; DstHeading = Heading.South } )
        //  Inner corner top right
        //  1.
        //  48
        //  Mask: 1 + 0 + 4 + 8 =  13
        Some ( { TraverseHeading = Heading.East;  Offset = (1, 0); SrcHeading = Heading.North; DstHeading = Heading.South },
               { TraverseHeading = Heading.North; Offset = (1, 0); SrcHeading = Heading.East;  DstHeading = Heading.West } )
        //  Inner corner top left
        //  .2
        //  48
        //  Mask: 0 + 2 + 4 + 8 =  14
        Some ( { TraverseHeading = Heading.North; Offset = (0, 0); SrcHeading = Heading.West;  DstHeading = Heading.East },
               { TraverseHeading = Heading.West;  Offset = (0, 0); SrcHeading = Heading.North; DstHeading = Heading.South } )
        //  Not an edge, IMPOSSIBLE
        //  12
        //  48
        //  Mask: 1 + 2 + 4 + 8 =  15
        None
    |]
        
    let areaToBitmask x y board =
        let bit x y = if get x y board = ' ' then 0 else 1
        (bit x y) ||| ((bit (x + 1) y) <<< 1) ||| ((bit x (y + 1)) <<< 2) ||| ((bit (x + 1) (y + 1)) <<< 3)
        
    let isCorner x y board =
        areaToBitmask x y board |> Int32.bitCount |> Int32.IsOddInteger
        
    
    let stitchPart board teleport2D innerCorner =
        // To stitch a part starting from an inner corner, here's what we do:
        //   1. Take the size of a face
        //   2. Take start X/Y
        //   3. Take CW endX/endY
        //   4. Take CCW endX/endY
        //   For each pair
        //     If we have not seen it, add it to teleport2D
        //     Continue with next side (if and only if there are not two corners)
        let getStitchDetails x y =
            match stitchLookup[areaToBitmask x y board] with
            | Some thing -> thing
            | None -> failwith $"This location cannot be traversed: ({x}, {y})"
            
        let faceSize = faceSize board
        let mutable (cwStartX, cwStartY) = innerCorner
        let mutable ccwStartX, ccwStartY = innerCorner
                        
        let mutable tp = teleport2D
        let mutable cont = true
        
        let add src dst map =
            if Map.containsKey src map then
                assert(dst = map[src])
            Map.add src dst map
        
        while cont do
            let (cw, _) = getStitchDetails cwStartX cwStartY
            let (_, ccw) = getStitchDetails ccwStartX ccwStartY
            for step = 0 to faceSize - 1 do
                let cwX = cwStartX + (dx[int cw.TraverseHeading] * step) + fst cw.Offset
                let cwY = cwStartY + (dy[int cw.TraverseHeading] * step) + snd cw.Offset
                let ccwX = ccwStartX + (dx[int ccw.TraverseHeading] * step) + fst ccw.Offset
                let ccwY = ccwStartY + (dy[int ccw.TraverseHeading] * step) + snd ccw.Offset
                tp <- add ((cwX,  cwY),  cw.SrcHeading)  ((ccwX, ccwY), ccw.DstHeading) tp
                tp <- add ((ccwX, ccwY), ccw.SrcHeading) ((cwX,  cwY),  cw.DstHeading)  tp
                
            // Set start values for the next iteration
            cwStartX  <- cwStartX + (dx[int cw.TraverseHeading] * faceSize)
            cwStartY  <- cwStartY + (dy[int cw.TraverseHeading] * faceSize)
            ccwStartX <- ccwStartX + (dx[int ccw.TraverseHeading] * faceSize)
            ccwStartY <- ccwStartY + (dy[int ccw.TraverseHeading] * faceSize)
                
            // Continue if and only if there are not two corners to traverse
            cont <- (not (isCorner cwStartX cwStartY board)) || (not (isCorner ccwStartX ccwStartY board))
        tp
        
    let stitch board =
        findInnerCorners board
        |> List.fold (stitchPart board) Map.empty
        
    let drawStitched board stitches =
        let stitched x y =
            stitches |> Map.exists (fun ((sx, sy), _heading) _dst -> x = sx && y = sy)
        for y = -1 to height board do
            for x = -1 to width board do
                if stitched x y then printf "+"
                else printf $"{get x y board}"
            printfn ""
        printfn ""
        
    let putMeBackOnCube stitches me board =
        let key = ((me.X, me.Y), enum me.Dir)
        if Map.containsKey key stitches then
            let ((x, y), dir) = stitches[key]
            let meJustOutsideCube = { X = x; Y = y; Dir = int dir }
            let meMovedInsideCube =
                { X = meJustOutsideCube.X + dx[meJustOutsideCube.Dir]
                  Y = meJustOutsideCube.Y + dy[meJustOutsideCube.Dir]
                  Dir = meJustOutsideCube.Dir }
            meMovedInsideCube
        else me
        
    [<AocSolver(2022, 22, Level = 2)>]
    let solve2 (input: string list) =
        let (board, instructions) = parse input
        let stitches = stitch board
        instructions
        |> Seq.scan (processInstruction (putMeBackOnCube stitches)) board
        |> Seq.last
        |> password
