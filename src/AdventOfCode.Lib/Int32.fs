module Int32

let parseOpt (str: string) =
    match System.Int32.TryParse(str) with
    | (true, value) -> Some value
    | (false, _) -> None
        
