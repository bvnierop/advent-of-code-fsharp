module AdventOfCode.Solutions._2023.Day24

open AdventOfCode.Lib.Solver
open FParsec
let pDecimal = pint64 |>> decimal
let pSep = pchar ',' .>> spaces
let pDecs = pipe3 (pDecimal .>> pSep) (pDecimal.>> pSep) pDecimal (fun a b c -> a, b, c)
let pLines = pDecs .>> spaces .>> pchar '@' .>> spaces .>>. pDecs
let parseLine = parseOrDie pLines

type Intersection = | No | Overlapping | Point of decimal * decimal
module Intersection =
    let unpack intersection =
        match intersection with
        | Point (x, y) -> Some (x, y)
        | _ -> None

    let isPoint intersection =
        match intersection with
        | Point _ -> true
        | _ -> false

    let isOverlapping intersection =
        match intersection with
        | Overlapping -> true
        | _ -> false

    let isNo intersection =
        match intersection with
        | No -> true
        | _ -> false

let intersection ((x1, y1, _z1), (vx1, vy1, _vz1)) ((x2, y2, _z2), (vx2, vy2, _vz2)) =
    if vx1 = 0m || vx2 = 0m then
        if vx1 = vx2 && x1 = x2 then Overlapping
        elif vx1 = vx2 && x1 <> x2 then No
        elif vx1 = 0m then
            let b = vy2 / vx2
            let yIntersect = b * (x1 - x2) + y2
            Point (x1, yIntersect)
        else // vx2 = 0m then
            let a = vy1 / vx1
            let yIntersect = a * (x2 - x1) + y1
            Point (x2, yIntersect)
    else
        let a = vy1 / vx1 // slope
        let c = y1 - a * x1 // y-intercept

        let b = vy2 / vx2
        let d = y2 - b * x2

        if a = b then
            if c = d then Overlapping
            else No
        else
            let ix = (d - c) / (a - b)
            let iy = a * ix + c
            if (ix > x1) = (vx1 > 0m) && (ix > x2) = (vx2 > 0m) then Point (ix, iy)
            else No


let intersectionYZ ((_x1, y1, z1), (_vx1, vy1, vz1)) ((_x2, y2, z2), (_vx2, vy2, vz2)) =
    if vy1 = 0m || vy2 = 0m then
        if vy1 = vy2 && y1 = y2 then Overlapping
        elif vy1 = vy2 && y1 <> y2 then No
        elif vy1 = 0m then
            let b = vz2 / vy2
            let zIntersect = b * (y1 - y2) + z2
            Point (y1, zIntersect)
        else // vy2 = 0m then
            let a = vz1 / vy1
            let zIntersect = a * (y2 - y1) + z1
            Point (y2, zIntersect)
    else
        let a = vz1 / vy1 // slope
        let c = z1 - a * y1 // z-intercept

        let b = vz2 / vy2
        let d = z2 - b * y2

        if a = b then
            if c = d then Overlapping
            else No
        else
            let iy = (d - c) / (a - b)
            let iz = a * iy + c
            if (iy > y1) = (vy1 > 0m) && (iy > y2) = (vy2 > 0m) then Point (iy, iz)
            else No

let insideArea ((ax1, ay1), (ax2, ay2)) (x, y) =
    x >= ax1 && x <= ax2 && y >= ay1 && y <= ay2


[<AocSolver(2023, 24, Level = 1)>]
let solve1 (input: string list) =
    let lines = input |> List.map parseLine

    let inputSize = List.length lines
    let targetMin, targetMax = if inputSize <= 100 then (7m, 27m) else (200000000000000m, 400000000000000m)

    List.pairs lines
    |> Seq.toList
    |> List.map ((fun (l1, l2) -> intersection l1 l2) >> Intersection.unpack)
    |> List.choose id
    |> List.filter (insideArea ((targetMin, targetMin), (targetMax, targetMax)))
    |> List.length

let adjustVelocity (dx, dy, dz) ((x, y, z), (vx, vy, vz)) = ((x, y, z), (vx - dx, vy - dy, vz - dz))

[<AocSolver(2023, 24, Level = 2)>]
let solve2 (input: string list) =
    let lines = input |> List.map parseLine
    (*
    Solving this the math way defeats me. Let's think about programming/trying my way out of this instead.
    One thing we can do that makes this easier, is to assume that the rock doesn't move. Then all lines other lines
    have to pass through a point. We can even do this on just two axis. We can also just use the first 5 or so hail
    stones. If we find a _unique_ solution for those, then we have a unique solution for all.

    So, basic idea:
    for x = -50 to 500
        for y = -500 to 500
            - adjust velocity of all lines
            - find the intersection of all lines
            - if they are all the same, we have a potential solution

    for each potential solution:
        for z = -50 to 500
            - adjust the velocity of all lines
            - find the intersection of all lines
            - if they are all the same, we have a solution

    *)
    // let lines = List.take (min 3 (List.length lines)) lines
    // for dx = -250 to 250 do
    //     for dy = -250 to 250 do
    //         let adjusted = List.map (adjustVelocity (decimal dx, decimal dy, 0m)) lines
    //         let intersections = List.pairs adjusted |> Seq.filter (fun (a, b) -> a <> b) |> Seq.toList |> List.map (fun (l1, l2) -> intersection l1 l2)
    //         // printfn $"{intersections}"
    //         let removeOverlapping = List.filter (fun i -> i <> Overlapping) intersections
    //         let allTheSame = (List.forall (Intersection.isNo >> not) removeOverlapping&& Set.ofList removeOverlapping |> Set.count = 1) || List.length removeOverlapping = 0
    //         if allTheSame then printf $"{dx}, {dy} -> {List.head intersections}\n"

    // If two lines have the same X-velo, then the rock needs a speed that satisfies:
    //   distance between line A and B = (RockVelo - HailVelo) = 0
    // So to get some speeds:
    //    Find all lines with the X velocity
    let n = 5000m
    let sameXVelos = List.groupBy (fun (_, (vx, _vy, _vz)) -> vx) lines
    let vx =
        [(-n) .. n]
        |> List.filter (fun potentialRockX ->
            sameXVelos
            |> List.forall (fun (hailVelocity, lines) ->
                let allLinePairs = List.pairs lines
                let satisfy = Seq.forall (fun (((x1, _y1, _z1), _v1), ((x2, _y2, _z2), _v2)) ->
                    let distance = abs (x1 - x2)
                    potentialRockX <> hailVelocity && distance % (potentialRockX - hailVelocity) = 0m) allLinePairs
                satisfy))
    let sameYVelos = List.groupBy (fun (_, (_vx, vy, _vz)) -> vy) lines
    let vy =
        [(-n) .. n]
        |> List.filter (fun potentialRockX ->
            sameYVelos
            |> List.forall (fun (hailVelocity, lines) ->
                let allLinePairs = List.pairs lines
                let satisfy = Seq.forall (fun (((_x1, y1, _z1), _v1), ((_x2, y2, _z2), _v2)) ->
                    let distance = abs (y1 - y2)
                    potentialRockX <> hailVelocity && distance % (potentialRockX - hailVelocity) = 0m) allLinePairs
                satisfy))
    let sameZVelos = List.groupBy (fun (_, (_vx, _vy, vz)) -> vz) lines
    let vz =
        [(-n) .. n]
        |> List.filter (fun potentialRockX ->
            sameZVelos
            |> List.forall (fun (hailVelocity, lines) ->
                let allLinePairs = List.pairs lines
                let satisfy = Seq.forall (fun (((_x1, _y1, z1), _v1), ((_x2, _y2, z2), _v2)) ->
                    let distance = abs (z1 - z2)
                    potentialRockX <> hailVelocity && distance % (potentialRockX - hailVelocity) = 0m) allLinePairs
                satisfy))
    // (vx, vy, vz)

    let rockVelocity = (List.head vx, List.head vy, List.head vz)
    let line1 = List.head lines |> adjustVelocity rockVelocity
    let line2 = List.head (List.tail lines) |> adjustVelocity rockVelocity
    let intersection1 = intersection line1 line2
    let intersection2 = intersectionYZ line1 line2
    let (xx, yy),( _yy, zz) = (Option.get (Intersection.unpack intersection1), Option.get (Intersection.unpack intersection2))
    System.Math.Round xx + System.Math.Round yy + System.Math.Round zz
