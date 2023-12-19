module AdventOfCode.Lib.Range

type 'a t = { Start: 'a; End: 'a }

let create low high = { Start = low; End = high }

let inline createWithLength start length = { Start = start; End = start + length }

let inline createEmpty () = { Start = LanguagePrimitives.GenericZero; End = LanguagePrimitives.GenericZero }

let inline contains number range = range.Start <= number && number < range.End

let inline last range = range.End - LanguagePrimitives.GenericOne

let disjoint range1 range2 =
    last range2 < range1.Start || last range1 < range2.Start

let intersection range1 range2 =
   create (max range1.Start range2.Start) (min range1.End range2.End)

let inline shift distance range = { Start = range.Start + distance; End = range.End + distance }

let inline split distance range =
    let first = { Start = range.Start; End = min (range.Start + distance) range.End }
    let second = { Start = min (range.Start + distance) range.End; End = range.End }
    (first, second)

let inline splitAt value range = split (value - range.Start) range

let inline length range = max (range.End - range.Start) LanguagePrimitives.GenericZero
let inline empty range = length range = LanguagePrimitives.GenericZero
let inline isEmpty range = length range = LanguagePrimitives.GenericZero

let inline difference range1 range2 =
    [ create range1.Start (min range1.End range2.Start)
      create (max range1.Start range2.End) range1.End ]
    |> List.filter (empty >> not)
