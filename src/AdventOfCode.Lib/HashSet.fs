module HashSet

open System.Collections.Immutable

type t<'a> = ImmutableHashSet<'a>
let empty<'a> = ImmutableHashSet<'a>.Empty
let size (set: 'a t) = set.Count
let add item (set: 'a t) = set.Add(item)
let remove item (set: 'a t) = set.Remove(item)
let contains item (set: 'a t) = set.Contains(item)
let ofSeq seq = seq |> Seq.fold (fun s e -> add e s) empty
let toSeq (set: t<'a>): 'a seq = set