namespace AdventOfCode.Solutions._2023

open AdventOfCode.Lib.Solver
open AdventOfCode.Lib

module Day05 =
    module AlmanacMap =
        type t = { SourceRange: int64 Range.t; DestinationStart: int64 }
        let make (ranges: (int64 * int64 * int64) list) =
            ranges
            |> List.map (fun (destStart, srcStart, length) ->
                { SourceRange = Range.createWithLength srcStart length; DestinationStart = destStart })

        let translateWithRanges (fromRange: int64 Range.t) (toRanges: t list) =
            let translateWithRanges' (fromRange: int64 Range.t) (toRange: t) =
                let intersection = Range.intersection fromRange toRange.SourceRange
                let differences = Range.difference fromRange toRange.SourceRange
                [Range.shift (toRange.DestinationStart - toRange.SourceRange.Start) intersection], differences
                |> List.reject Range.empty
                |> List.sort
            let translated, untranslated =
                List.fold (fun (translatedRanges, untranslatedRanges) toRange ->
                    let translatedRanges', untranslatedRanges' =
                        List.fold (fun (translatedRanges, untranslatableRanges) nextUntranslatedRange ->
                            let translatedRanges', untranslatableRanges' = translateWithRanges' nextUntranslatedRange toRange
                            translatedRanges @ translatedRanges', untranslatableRanges @ untranslatableRanges') ([], []) untranslatedRanges
                    translatedRanges @ translatedRanges', untranslatedRanges') ([], [fromRange]) toRanges
            translated @ untranslated |> List.reject Range.empty |> List.sort

    module Almanac =
        open FParsec

        type t = { Seeds: int64 list; Ranges: AlmanacMap.t list list }
        let make seeds mapRanges = { Seeds = seeds; Ranges = mapRanges }

        let translateRange (sourceRange: int64 Range.t) (almanac: t) =
            List.fold (fun rangeList ranges ->
                    List.collect (fun range ->
                        AlmanacMap.translateWithRanges range ranges) rangeList)
                [sourceRange] almanac.Ranges

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
        |> List.map (fun s -> Range.create s (s + 1L))
        |> List.collect (fun r -> Almanac.translateRange r almanac)
        |> List.min
        |> (fun r -> r.Start)

    [<AocSolver(2023, 5, Level = 2)>]
    let solve2 (input: string) =
        let almanac = Almanac.parse input

        almanac.Seeds
        |> List.chunkBySize 2
        |> List.map (fun pair -> let arr = pair |> Array.ofList in Range.create arr[0] (arr[0] + arr[1]))
        |> List.collect (fun r -> Almanac.translateRange r almanac)
        |> List.min
        |> (fun r -> r.Start)
