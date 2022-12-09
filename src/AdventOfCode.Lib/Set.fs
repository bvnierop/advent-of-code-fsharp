module Set

let sum set = Set.fold (+) 0 set

let length (set: Set<'a>) = Seq.length set

