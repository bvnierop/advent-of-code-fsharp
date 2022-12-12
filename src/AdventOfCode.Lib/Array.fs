module Array

let findIndex2D predicate (array: 'a array array) =
    Seq.pick id <| seq {
        for y = 0 to Array.length array - 1 do
            for x = 0 to Array.length array[y] - 1 do
                if predicate array.[y].[x] then yield Some (y, x)
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
