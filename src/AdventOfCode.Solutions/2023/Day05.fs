namespace AdventOfCode.Solutions._2023

open AdventOfCode.Lib.Solver
open System

module Day05 =
    module Range =
        type 'a t = { Start: 'a; End: 'a }
        let makeWithLength start length = { Start = start; End = start + length - 1L }
        let contains number range = range.Start <= number && number <= range.End

    module AlmanacMap =
        type t = { SourceRange: int64 Range.t; DestinationStart: int64 }
        let make (ranges: (int64 * int64 * int64) list) =
            ranges
            |> List.map (fun (destStart, srcStart, length) ->
                { SourceRange = Range.makeWithLength srcStart length; DestinationStart = destStart })

        let translate (source: int64) (map: t list) =
            match List.tryFind (fun mapRange -> Range.contains source mapRange.SourceRange) map with
            | Some mapRange -> source - mapRange.SourceRange.Start + mapRange.DestinationStart
            | None -> source

    module Almanac =
        open FParsec

        type t = { Seeds: int64 list; Ranges: AlmanacMap.t list list }
        let make seeds mapRanges = { Seeds = seeds; Ranges = mapRanges }

        let translate source (almanac: t) = List.fold AlmanacMap.translate source almanac.Ranges

        let pSeeds = pstring "seeds: " >>. sepBy pint64 (pchar ' ') .>> skipNewline .>> skipNewline
        let pAlmanacMapLine = pint64 .>> spaces .>>. pint64 .>> spaces .>>. pint64 .>> (skipNewline <|> eof)
        let pAlmanacMap = manyCharsTill anyChar (pchar ':') >>. skipNewline >>. manyTill pAlmanacMapLine (skipNewline <|> eof)
        let pAlmanac = pSeeds .>>. manyTill pAlmanacMap eof
        let parse str =
            let seeds, ranges = parseOrDie pAlmanac str
            let typed =
                ranges
                |> List.map (fun range -> List.map (fun ((dst, src), len) -> (dst, src, len)) range |> AlmanacMap.make)
            { Seeds = seeds; Ranges = typed }

    [<AocSolver(2023, 5, Level = 1)>]
    let solve1 (input: string) =
        let almanac = Almanac.parse input
        almanac.Seeds
        |> List.map (fun s -> Almanac.translate s almanac)
        |> List.min

    [<AocSolver(2023, 5, Level = 2)>]
    let solve2 (input: string) =
        let almanac = Almanac.parse input
        let inline longFor low high f =
            let rec loop n =
                if n < high then f n; loop (n + 1L)
            loop low

        let mutable closest = Int64.MaxValue
        for pair in List.chunkBySize 2 almanac.Seeds do
            let pairArray = pair |> Array.ofList
            longFor pairArray[0] (pairArray[0] + pairArray[1]) (fun seed ->
                closest <- min closest (Almanac.translate seed almanac))
        closest
