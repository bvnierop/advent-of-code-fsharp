module AdventOfCode.Test._2023.Day15

open System
open Xunit
open Swensen.Unquote
open AdventOfCode.Solutions._2023.Day15

[<Theory>]
[<InlineData("rn=1", 30)>]
[<InlineData("cm-", 253)>]
[<InlineData("qp=3", 97)>]
[<InlineData("cm=2", 47)>]
[<InlineData("qp-", 14)>]
[<InlineData("pc=4", 180)>]
[<InlineData("ot=9", 9)>]
[<InlineData("ab=5", 197)>]
[<InlineData("pc-", 48)>]
[<InlineData("pc=6", 214)>]
[<InlineData("ot=7", 231)>]
let ``calculate hash code`` src expected = test <@ HASH src = expected @>

[<Fact>]
let ``parse input`` () =
    test <@ parse "foo,bar,baz\n" = [ "foo"; "bar"; "baz" ] @>


[<Fact>]
let ``parse input 2`` () =
    test
        <@
            Instruction.parse "rn=1,qp-,xyz=3\n" = [ { Label = "rn"; Operation = Set 1 }
                                                     { Label = "qp"; Operation = Remove }
                                                     { Label = "xyz"; Operation = Set 3 } ]
        @>

let testInstructionProcessing input expected =
    let instructions = Instruction.parse input
    test <@
            List.fold Instruction.applyToHashMap Map.empty instructions = expected
        @>

[<Fact>]
let ``add a lens to an empty box`` () =
    testInstructionProcessing "rn=1" (Map [ (0, [ ("rn", 1) ]) ])

[<Fact>]
let ``Update focus length of a lens`` () =
    testInstructionProcessing "rn=1,rn=2" (Map [ (0, [ ("rn", 2) ]) ])

[<Fact>]
let ``add second lens to a box`` () =
    testInstructionProcessing "rn=1,cm=2" (Map [ (0, [ ("cm", 2); ("rn", 1) ]) ])

[<Fact>]
let ``update focus length of a lens when multiple lenses are in the box`` () =
    testInstructionProcessing "rn=1,cm=2,rn=3,cm=4,rn=5" (Map [ (0, [ ("cm", 4); ("rn", 5) ]) ])

[<Fact>]
let ``remove lens from empty box`` () =
    testInstructionProcessing "rn-" Map.empty

[<Fact>]
let ``remove lens from a box with just that value in it`` () =
    testInstructionProcessing "rn=3,rn-" (Map [ (0, []) ])

[<Fact>]
let ``remove lens from a box with multiple values`` () =
    testInstructionProcessing "ot=3,ab=5,pc=7,ab-" (Map [ (3, [ ("pc", 7); ("ot", 3) ]) ])

[<Fact>]
let ``calculate focus length`` () =
    let instructions = Instruction.parse "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"
    let hashmap = List.fold Instruction.applyToHashMap Map.empty instructions
    test <@ focusingPower hashmap = 145 @>

[<Fact>]
let ``solve part 2`` () =
    test <@ solve2 "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7" = 145 @>
