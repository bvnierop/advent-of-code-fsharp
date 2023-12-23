module Queue

open System.Collections.Immutable

let empty<'a> = ImmutableQueue<'a>.Empty
let dequeue (queue: ImmutableQueue<'a>) = queue.Dequeue()
let enqueue elt (queue: ImmutableQueue<'a>) = queue.Enqueue(elt)
let singleton<'a> elt = empty<'a> |> enqueue elt
let enqueueMany elts (queue: ImmutableQueue<'a>) =
    List.fold (fun q e -> enqueue e q) queue elts
let front (queue: ImmutableQueue<'a>) = queue.Peek()
let isEmpty (queue: ImmutableQueue<'a>) = queue.IsEmpty
