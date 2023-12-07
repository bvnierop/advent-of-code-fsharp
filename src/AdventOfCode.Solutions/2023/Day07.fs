namespace AdventOfCode.Solutions._2023

open System
open AdventOfCode.Lib.Solver
open FParsec

module Day07 =
    type Face =
        | Wildcard
        | One | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
    module Face =
        let ofChar1 = function
            | '1' -> One | '2' -> Two | '3' -> Three | '4' -> Four | '5' -> Five | '6' -> Six | '7' -> Seven
            | '8' -> Eight | '9' -> Nine | 'T' -> Ten | 'J' -> Jack | 'Q' -> Queen | 'K' -> King | 'A' -> Ace
            | _ -> failwith "Invalid face"
        let ofChar2 = function
            | '1' -> One | '2' -> Two | '3' -> Three | '4' -> Four | '5' -> Five | '6' -> Six | '7' -> Seven
            | '8' -> Eight | '9' -> Nine | 'T' -> Ten | 'J' -> Wildcard | 'Q' -> Queen | 'K' -> King | 'A' -> Ace
            | _ -> failwith "Invalid face"

    type HandType = HighCard | OnePair | TwoPair | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind

    let identifyHand hand =
        let wildcardCount = List.countWhere (fun f -> f = Wildcard) hand
        let grouped =
            hand
            |> List.reject (fun f -> f = Wildcard)
            |> List.countBy id
            |> List.sortBy (fun (_k, v) -> -v)
            |> function (k, v) :: xs -> (k, v + wildcardCount) :: xs | [] -> [Wildcard, wildcardCount]
        match grouped with
        | [(_, 5)] -> FiveOfAKind
        | [(_, 4); (_, 1)] -> FourOfAKind
        | [(_, 3); (_, 2)] -> FullHouse
        | [(_, 3); (_, 1); (_, 1)] -> ThreeOfAKind
        | [(_, 2); (_, 2); (_, 1)] -> TwoPair
        | [(_, 2); (_, 1); (_, 1); (_, 1)] -> OnePair
        | _ -> HighCard

    [<CustomComparison; CustomEquality>]
    type CamelHand =
        { Hand: Face list; Bid: int }
        static member compare a b =
            let handA = identifyHand a.Hand
            let handB = identifyHand b.Hand
            if handA <> handB then Operators.compare handA handB
            else Operators.compare a.Hand b.Hand

        interface IComparable with
            member a.CompareTo b =
                match b with
                | :? CamelHand as ch -> CamelHand.compare a ch
                | _ -> invalidArg "b" "Cannot compare values of different types"

        override this.Equals other =
            match other with
            | :? CamelHand as ch -> CamelHand.compare this ch = 0
            | _ -> false

        override this.GetHashCode() = hash this

    let pHand = anyString 5 .>> spaces .>>. pint32 .>> eof
    let parseLine faceParser str =
        parseOrDie pHand str
        ||> (fun hand bid -> { Hand = hand |> String.toArray |> Array.toList |> List.map faceParser ; Bid = bid })

    let solveForFaceSet parser input =
        input
        |> List.map (parseLine parser)
        |> List.sort
        |> List.indexed
        |> List.fold (fun total (rank, hand) -> total + (hand.Bid * (rank + 1))) 0

    [<AocSolver(2023, 7, Level = 1)>]
    let solve1 (input: string list) = solveForFaceSet Face.ofChar1 input

    [<AocSolver(2023, 7, Level = 2)>]
    let solve2 (input: string list) = solveForFaceSet Face.ofChar2 input
