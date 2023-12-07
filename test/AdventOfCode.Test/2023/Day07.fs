module AdventOfCode.Test._2023.Day07

open System
open System.Threading
open Xunit
open Swensen.Unquote
open AdventOfCode.Solutions._2023.Day07

[<Fact>]
let ``sort faces`` () =
    [ Three; Ace; Two; Four; ]
    |> List.sort =! [ Two; Three; Four; Ace ]

let handGenerator () : obj array seq = seq {
    yield [| [Ace; Ace; Ace; Ace; Ace]; FiveOfAKind |]
    yield [| [Ace; Ace; Ace; Ace; Two]; FourOfAKind |]
    yield [| [Ace; Ace; Ace; Two; Two]; FullHouse |]
    yield [| [One; Two; Ace; Ace; Ace]; ThreeOfAKind |]
    yield [| [One; One; Two; Two; Three]; TwoPair |]
    yield [| [One; One; Two; Three; Four]; OnePair |]
    yield [| [One; Two; Three; Four; Five]; HighCard |]

    // With wildcards
    yield [| [Wildcard; Wildcard; Wildcard; Wildcard; Wildcard]; FiveOfAKind |]
    yield [| [Ace; Ace; Ace; Ace; Wildcard]; FiveOfAKind |]
    yield [| [Ace; Ace; Ace; Wildcard; Wildcard]; FiveOfAKind |]
}

[<Theory>]
[<MemberData(nameof(handGenerator))>]
let ``identify hand`` hand expected =
    identifyHand hand =! expected

[<Fact>]
let ``parse line`` () =
    parseLine Face.ofChar1 "32T3K 765"  =
        { Hand = [Three; Two; Ten; Three; King]; Bid = 765 }

let testInput =
    [
        "32T3K 765";
        "T55J5 684";
        "KK677 28";
        "KTJJT 220";
        "QQQJA 483"
    ]

[<Fact>]
let ``sorts hands`` () =
    let hands =
        testInput
        |> List.map (parseLine Face.ofChar1)

    List.sort hands =! [
        { Hand = [ Three; Two; Ten; Three; King ]; Bid = 765 }
        { Hand = [ King; Ten; Jack; Jack; Ten ]; Bid = 220 }
        { Hand = [ King; King; Six; Seven; Seven ]; Bid = 28 }
        { Hand = [ Ten; Five; Five; Jack; Five ]; Bid = 684 }
        { Hand = [ Queen; Queen; Queen; Jack; Ace ]; Bid = 483 }
    ]

[<Fact>]
let ``solve part 1`` () =
    solve1 testInput =! 6440

[<Fact>]
let ``solve part 2`` () =
    solve2 testInput =! 5905
