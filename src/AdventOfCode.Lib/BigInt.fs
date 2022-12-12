module BigInt

open System.Numerics

let parseOpt (str: string) =
    match BigInteger.TryParse(str) with
    | (true, value) -> Some value
    | (false, _) -> None
        
let parseChr (chr: char) = BigInteger.Parse($"{chr}")

let parse x = BigInteger.Parse(x)
   

