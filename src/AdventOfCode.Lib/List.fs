module List 
let countWhere predicate = List.filter predicate >> List.length
