module Array2D
let foldi (folder: int -> int -> 'S -> 'T -> 'S) (state: 'S) (array: 'T[,]) =
    let mutable state = state
    let b1 = Array2D.base1 array
    let b2 = Array2D.base2 array
    for x in b1 .. b1 + Array2D.length1 array - 1 do
        for y in b2 .. b2 + Array2D.length2 array - 1 do
            state <- folder x y state (array.[x, y])
    state

let fold (folder: 'S -> 'T -> 'S) (state: 'S) (array: 'T[,]) =
    let mutable state = state
    let b1 = Array2D.base1 array
    let b2 = Array2D.base2 array
    for x in b1 .. b1 + Array2D.length1 array - 1 do
        for y in b2 .. b2 + Array2D.length2 array - 1 do
            state <- folder state (array.[x, y])
    state
