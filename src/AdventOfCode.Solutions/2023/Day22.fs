module AdventOfCode.Solutions._2023.Day22

open AdventOfCode.Lib.Solver
open FParsec

(*
1205 items -> n*n is possible

Coordinates occupy a full block:
2,2,2~2,2,2 -> one full block

Block don't overlap if:
 - a.maxX < b.minX (because a block that ends at X=2 goes over a block that begins at X=2)
 - a.minX >= b.maxX (because a block that begins at X=2 goes next to a block that ends at X=2)

 Blocks can have various heights.

 Step 1: Drop the blocks
    In order to drop the blocks, we need to first order them by Z order
    Then, look at each block that we have already processed, finding the highest Z that overlaps.
    Drop the brick on that Z.
 Step 2: For each block, find how many blocks support it
    This means we need to find the blocks that are directly below it
    If a block is directly below it, it supports it
 Step 3: For each block, find which blocks it supports
    Does this mean blocks need an ID?
 Step 4: If a block only supports blocks with more than 1 support, it can be disintegrated


 A brick can be disintegrated if all bricks it supports have at least one other support
    n * n * n is too expensive, probably, but we can cache later

PART 2

For each brick, determine how many bricks fall if this one is disintegrated. Sum the results.

(OPTIMIZATION NOTE: WE CAN MEMOIZE THE RESULTS IF NEEDED):
    If A causes F to fall, and F causes G to fall, then we only need to compute F once,
    and just look it up the second time.

We'll pass a list of settled bricks and assume that the first one will be disintegrated. Then:
  For each other brick
    if all of its supporting bricks have been disintegrated OR fallen
        then it falls (so mark it)
*)

let pCoord = pipe3 (pint32 .>> pchar ',') (pint32 .>> pchar ',') pint32 (fun x y z -> (x,y,z))
let pBlock = pipe2 (pCoord .>> pstring "~") pCoord  (fun a b -> (a,b))
let parse = parseOrDie pBlock

let disjointXY ((minX1, minY1, minZ1), (maxX1, maxY1, maxZ1)) ((minX2, minY2, minZ2), (maxX2, maxY2, maxZ2)) =
    minX2 > maxX1 || maxX2 < minX1 || minY2 > maxY1 || maxY2 < minY1

let overlapXY block1 block2 =
    not (disjointXY block1 block2)

let height ((_, _, minZ), (_, _, maxZ)) = maxZ - minZ + 1
let width ((minX, _, _), (maxX, _, _)) = maxX - minX + 1
let depth ((_, minY, _), (_, maxY, _)) = maxY - minY + 1
let inline X1 ((x1, _, _), (_, _, _)) = x1
let inline Y1 ((_, y1, _), (_, _, _)) = y1
let inline Z1 ((_, _, z1), (_, _, _)) = z1
let inline X2 ((_, _, _), (x2, _, _)) = x2
let inline Y2 ((_, _, _), (_, y2, _)) = y2
let inline Z2 ((_, _, _), (_, _, z2)) = z2

let moveTo (x, y, z) ((minX, minY, minZ), (maxX, maxY, maxZ)) =
    let width = maxX - minX + 1
    let depth = maxY - minY + 1
    let height = maxZ - minZ + 1
    ((x, y, z), (x + width - 1, y + depth - 1, z + height - 1))


//    n * n * n is too expensive, probably, but we can cache later

// We support all bricks that are one Z level above us that overlap
let supports upperBrick lowerBrick =
    Z1 upperBrick - Z2 lowerBrick = 1 && overlapXY upperBrick lowerBrick
// We are supported by all bricks that are one Z level below us that overlap
let supportedBy lowerBrick upperBrick = supports upperBrick lowerBrick

// A brick can be disintegrated if all bricks it supports have at least one other support
let canBeDisintegrated bricks brick =
    // find all bricks that are supported by this brick
    let supportedBricks = List.filter (supportedBy brick) bricks

    List.forall (fun supportedBrick ->
        // find all bricks that support this brick
        let supportingBricks = List.filter (supports supportedBrick) bricks
        // make sure that there is at least one other brick that supports this brick
        let res = List.exists (fun supportingBrick -> supportingBrick <> brick) supportingBricks
        res
    ) supportedBricks

// We'll pass a list of settled bricks and assume that the first one will be disintegrated. Then:
//   For each other brick
//     if all of its supporting bricks have been disintegrated OR fallen
//         then it falls (so mark it)
let rec countFallingBricks settledBricks brick =
    let rec loop bricks gone =
        match bricks with
        | [] -> Set.length gone - 1 // We don't count the first brick
        | brick::bricks ->
            let supportingBricks = List.filter (supports brick) settledBricks |> Set.ofList
            let supportingBricksRemaining = Set.difference supportingBricks gone
            if Set.isEmpty supportingBricks then loop bricks gone // This is a ground brick. It doesn't fall.
            elif Set.isEmpty supportingBricksRemaining then loop bricks (Set.add brick gone)
            else loop bricks gone

    let skip = List.findIndex (fun b -> b = brick) settledBricks
    loop (List.skip (skip + 1) settledBricks) (Set.ofList [brick])

let show bricks =
    List.iter (fun brick ->
        let (minX, minY, minZ), (maxX, maxY, maxZ) = brick
        printfn "%d,%d,%d~%d,%d,%d" minX minY minZ maxX maxY maxZ
    ) bricks
    bricks

let settle bricks =
    let rec loop remainingBricks settledBricks =
        match remainingBricks with
        | [] -> List.rev settledBricks
        | brick::remainingBricks ->
            let overlappingBricks = List.reject (disjointXY brick) settledBricks
            let newBrickPosition =
                match overlappingBricks with
                | [] -> moveTo (X1 brick, Y1 brick, 0)
                | _ ->
                    let _, (_, _, highestZ) = List.maxBy (fun (_, (_, _, z2)) -> z2) overlappingBricks
                    moveTo (X1 brick, Y1 brick, highestZ + 1)
            loop remainingBricks (newBrickPosition brick :: settledBricks)
    loop (bricks |> List.sortBy (fun ((_, _, z1), (_, _, _)) -> z1)) []

[<AocSolver(2023, 22, Level = 1)>]
let solve1 (input: string list) =
    let bricks = List.map parse input
    let settled = settle bricks

    List.filter (canBeDisintegrated settled) settled
    |> List.length


[<AocSolver(2023, 22, Level = 2)>]
let solve2 (input: string list) =
    let bricks = List.map parse input
    let settled = settle bricks

    [0..List.length settled - 1]
    |> List.map (fun i -> countFallingBricks settled (List.item i settled))
    |> List.sum
