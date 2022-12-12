module Int64

let parseOpt (str: string) =
    match System.Int64.TryParse(str) with
    | (true, value) -> Some value
    | (false, _) -> None
        
let parseChr (chr: char) = System.Int64.Parse($"{chr}")

let parse x = System.Int64.Parse(x)
   
