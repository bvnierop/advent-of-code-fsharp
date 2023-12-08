module AdventOfCode.Lib.Math

let rec gcd a b =
    if b = LanguagePrimitives.GenericZero then a
    elif a = LanguagePrimitives.GenericZero then b
    else gcd b (a % b)

let rec gcd64 a b =
    if b = 0L then a
    elif a = 0L then b
    else gcd64 b (a % b)

let inline lcm64 a b = a * b / (gcd64 a b)

let inline lcm a b = a * b / (gcd a b)

let inline modE a b = ((a % b) + b) % b
