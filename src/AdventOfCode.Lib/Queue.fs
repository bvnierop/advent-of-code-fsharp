module Queue

open System.Collections.Immutable

let empty () = ImmutableQueue.Empty
let dequeue (queue: ImmutableQueue<'a>) = queue.Dequeue()
let enqueue elt (queue: ImmutableQueue<'a>) = queue.Enqueue(elt)
let front (queue: ImmutableQueue<'a>) = queue.Peek()
let isEmpty (queue: ImmutableQueue<'a>) = queue.IsEmpty


