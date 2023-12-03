module Set

let sum set = Set.fold (+) 0 set
let sumBy fn set = Set.map fn set |> sum

let length (set: Set<'a>) = Seq.length set
