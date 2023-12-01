module String

open System

let isNullOrWhiteSpace (str: string) = String.IsNullOrWhiteSpace(str)
let isNullOrEmpty (str: string) = String.IsNullOrEmpty(str)
let split (str: string) = str.Split()
let splitOn on (str: string) = str.Split(on)
let splitBy (substr: string) (str: string) = str.Split(substr)
let joinSeq (separator: string) (sequence: 'a seq) = String.Join(separator, sequence)
let padRight totalLength (str: string) = str.PadRight(totalLength)
