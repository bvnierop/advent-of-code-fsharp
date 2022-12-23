module Int32

let parseOpt (str: string) =
    match System.Int32.TryParse(str) with
    | (true, value) -> Some value
    | (false, _) -> None
        
let parseChr (chr: char) = System.Int32.Parse($"{chr}")

let parse x = System.Int32.Parse(x)
   
let inline bitCount (n : int) =
    let count2 = n - ((n >>> 1) &&& 0x55555555)
    let count4 = (count2 &&& 0x33333333) + ((count2 >>> 2) &&& 0x33333333)
    let count8 = (count4 + (count4 >>> 4)) &&& 0x0f0f0f0f
    (count8 * 0x01010101) >>> 24