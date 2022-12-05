module FParsec

open FParsec

let parseOrDie parser input =
    match run parser input with
    | Success(res, _, _) -> res
    | Failure(errorString, _, _) -> printfn $"Parsing failed: {errorString}"; exit(-1)
