module Array

let last (arr: _[]) = arr[arr.Length - 1]
let first (arr: _[]) = arr[0]

let getDefault value index array =
    if index >= 0 && index < Array.length array then array[index]
    else value

let getDefault2D value index1 index2 array =
    if index1 < 0 || index1 >= Array.length array then value
    else
        let row = array[index1]
        if index2 < 0 || index2 >= Array.length row then value
        else row[index2]

let findIndex2D predicate (array: 'a array array) =
    Seq.pick id <| seq {
        for y = 0 to Array.length array - 1 do
            for x = 0 to Array.length array[y] - 1 do
                if predicate array[y].[x] then yield Some (y, x)
                else yield None
    }

let neighbours index1 index2 (array: 'a array array) = seq {
    let deltas = [(0, 1); (0, -1); (1, 0); (-1, 0)]
    for (di1, di2) in deltas do
        let newI1 = index1 + di1
        let newI2 = index2 + di2
        if newI1 >= 0 && newI1 < Array.length array &&
           newI2 >= 0 && newI2 < Array.length array[newI1] then yield (newI1, newI2)
}

let neighbours8 index1 index2 (array: 'a array array) = seq {
    for dx in [-1 .. 1] do
        for dy in [-1 .. 1] do
            let newI1 = index1 + dx
            let newI2 = index2 + dy
            if (dx <> 0 || dy <> 0) &&
               newI1 >= 0 && newI1 < Array.length array &&
               newI2 >= 0 && newI2 < Array.length array[newI1] then yield (newI1, newI2)
}

let fold2D folder initialState arr =
    Array.fold (fun state row ->
        Array.fold (fun state cell -> folder state cell) state row
    ) initialState arr

let foldi folder initialState arr =
    Array.indexed arr
    |> Array.fold (fun state (index, column) -> folder state index column) initialState

let foldi2D folder initialState arr =
    foldi (fun state rowIndex ->
        foldi (fun state colIndex cell -> folder state (rowIndex, colIndex) cell) state
    ) initialState arr
