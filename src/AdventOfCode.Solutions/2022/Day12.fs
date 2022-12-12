namespace AdventOfCode.Solutions._2022

open System.Collections.Immutable
open AdventOfCode.Lib.Solver
open System

module Queue =
    let empty () = ImmutableQueue.Empty
    let dequeue (queue: ImmutableQueue<'a>) = queue.Dequeue()
    let enqueue elt (queue: ImmutableQueue<'a>) = queue.Enqueue(elt)
    let front (queue: ImmutableQueue<'a>) = queue.Peek()
    let isEmpty (queue: ImmutableQueue<'a>) = queue.IsEmpty

module Day12 =
    // Parse input
    //   Split into chars (which it already is cause String == Seq
    //   Convert to ints
    //     if S -> 0, if E -> 25, else Convert.toInt(chr) - Convert.toInt('a')
    //   Convert to Array2D (or Array of Arrays)
    // BFS
    //  Start on S
    //  End when E is reached
    //  Use .NET's immutable queue
    //   But write some wrappers
    let rec parseChr = function
        | 'S' -> parseChr 'a'
        | 'E' -> parseChr 'z'
        | c -> Convert.ToInt32(c) - Convert.ToInt32('a')
        
    let parseLine line = line |> Array.map parseChr
    let parse (input: string list) =
        let asArray = input |> List.map Seq.toArray |> List.toArray
        (Array.findIndex2D ((=) 'S') asArray, Array.findIndex2D ((=) 'E') asArray,
         asArray |> Array.map parseLine)
    
    [<AocSolver(2022, 12, Level = 1)>]
    let solve1 (input: string list) =
        let (start, finish, grid) = parse input
        
        let q = Queue.enqueue (0, start) (Queue.empty ())
        let seen = Set.add start Set.empty
        let rec loop q seen =
            if Queue.isEmpty q then None // No answer found
            else
                let (dist, pos) = Queue.front q
                if pos = finish then Some dist
                else
                    let (newQ, newSeen) =
                        Seq.fold (fun (newQ, newSeen) npos ->
                            let h = grid.[fst pos].[snd pos]
                            let newh = grid.[fst npos].[snd npos]
                            if not <| Set.contains npos newSeen && newh <= h + 1 then
                                (Queue.enqueue (dist + 1, npos) newQ,
                                Set.add npos newSeen)
                            else
                                (newQ, newSeen)) <| (Queue.dequeue q, seen) <| (Array.neighbours <|| pos <| grid)
                    loop newQ newSeen
                    
        loop q seen |> Option.defaultValue -1
                
        
    [<AocSolver(2022, 12, Level = 2)>]
    let solve2 (input: string list) =
        let (start, finish, grid) = parse input
        
        let q = Queue.enqueue (0, finish) (Queue.empty ())
        let seen = Set.add start Set.empty
        let rec loop q seen =
            if Queue.isEmpty q then None // No answer found
            else
                let (dist, (p1, p2)) = Queue.front q
                if grid.[p1].[p2] = 0 then Some dist
                else
                    let (newQ, newSeen) =
                        Seq.fold (fun (newQ, newSeen) npos ->
                            let h = grid.[p1].[p2]
                            let newh = grid.[fst npos].[snd npos]
                            if not <| Set.contains npos newSeen && h - newh <= 1 then
                                (Queue.enqueue (dist + 1, npos) newQ,
                                Set.add npos newSeen)
                            else
                                (newQ, newSeen)) <| (Queue.dequeue q, seen) <| (Array.neighbours <| p1 <| p2 <| grid)
                    loop newQ newSeen
                    
        loop q seen |> Option.defaultValue -1
