module Array2D
let foldi (folder: int -> int -> 'S -> 'T -> 'S) (state: 'S) (array: 'T[,]) =
    let mutable state = state
    let b1 = Array2D.base1 array
    let b2 = Array2D.base2 array
    for y in b1 .. b1 + Array2D.length1 array - 1 do
        for x in b2 .. b2 + Array2D.length2 array - 1 do
            state <- folder x y state (array.[y, x])
    state

let fold (folder: 'S -> 'T -> 'S) (state: 'S) (array: 'T[,]) =
    let mutable state = state
    let b1 = Array2D.base1 array
    let b2 = Array2D.base2 array
    for x in b1 .. b1 + Array2D.length1 array - 1 do
        for y in b2 .. b2 + Array2D.length2 array - 1 do
            state <- folder state (array.[x, y])
    state

let findIndex (predicate: 'T -> bool) (array: 'T[,]) =
    let b1 = Array2D.base1 array
    let b2 = Array2D.base2 array
    let maxRow = b1 + Array2D.length1 array - 1
    let maxCol = b2 + Array2D.length2 array - 1
    let rec loop x y =
        if array[y, x] |> predicate then
            Some (x, y)
        elif x = maxCol then
            if y = maxRow - 1 then
                None
            else
                loop 0 (y + 1)
        else
            loop (x + 1) y
    loop b2 b1

let neighbours4 x y (array: 'a[,]) = seq {
    let b1 = Array2D.base1 array
    let b2 = Array2D.base2 array

    let deltas = [(0, 1); (0, -1); (1, 0); (-1, 0)]
    for di1, di2 in deltas do
        let newy = y + di1
        let newx = x + di2
        if newy >= b1 && newy < Array2D.length1 array + b1 &&
           newx >= 0 && newx < Array2D.length2 array + b2 then yield (newx, newy)
}

let neighbouringValues4 x y (array: 'a[,]) =
    neighbours4 x y array
    |> Seq.map (fun (x, y) -> array[y,x])

let neighbours8 x y (array: 'a[,]) = seq {
    let b1 = Array2D.base1 array
    let b2 = Array2D.base2 array

    for dx in [-1 .. 1] do
        for dy in [-1 .. 1] do
            let newy = x + dx
            let newx = y + dy
            if (dx <> 0 || dy <> 0) &&
                newy >= b1 && newy < Array2D.length1 array + b1 &&
                newx >= 0 && newx < Array2D.length2 array + b2 then yield (newx, newy)
}

let neighbouringValues8 x y (array: 'a[,]) =
    neighbours8 x y array
    |> Seq.map (fun (x, y) -> array[y, x])

let getDefault x y defaultValue (array: 'a[,]) =
    let b1 = Array2D.base1 array
    let b2 = Array2D.base2 array
    if y >= b1 && y < Array2D.length1 array + b1 &&
       x >= b2 && x < Array2D.length2 array + b2 then array[y, x]
    else defaultValue

let minRowIndex (array: 'a[,]) = Array2D.base1 array
let maxRowIndex (array: 'a[,]) = Array2D.base1 array + Array2D.length1 array - 1
let minColumnIndex (array: 'a[,]) = Array2D.base2 array
let maxColumnIndex (array: 'a[,]) = Array2D.base2 array + Array2D.length2 array - 1
let rowIndices (array: 'a[,]) =
    [ minRowIndex array .. maxRowIndex array ]
let columnIndices (array: 'a[,]) =
    [ minColumnIndex array .. maxColumnIndex array ]

let borderIndices (array: 'a[,]) =
    let minRow = minRowIndex array
    let maxRow = maxRowIndex array
    let minCol = minColumnIndex array
    let maxCol = maxColumnIndex array
    [
        for x in minCol .. maxCol do
            yield x, minRow
            yield x, maxRow
        for y in minRow .. maxRow do
            yield minCol, y
            yield maxCol, y
    ]
