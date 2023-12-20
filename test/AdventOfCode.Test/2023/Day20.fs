module AdventOfCode.Test._2023.Day20

open System
open Xunit
open Swensen.Unquote
open AdventOfCode.Solutions._2023.Day20

(*
broadcaster -> a, b, c
%a -> b
%b -> c
%c -> inv
&inv -> a

-----

broadcaster -> a
%a -> inv, con
&inv -> b
%b -> con
&con -> output
*)

[<Fact>]
let ``parse input`` () =
    test <@ Module.parseLine "broadcaster -> a, b" = { Id = "broadcaster"; Adjacent = [ "a"; "b" ]; State =  BroadcasterModuleState } @>
    test <@ Module.parseLine "%flip -> flop" = { Id = "flip"; Adjacent = ["flop"]; State = FlipFlopModuleState Off } @>
    test <@ Module.parseLine "&con -> j" = { Id = "con"; Adjacent = ["j"]; State = ConjunctionModuleState Map.empty } @>

[<Fact>]
let ``flip flop module does nothing when receiving a high pulse`` () =
    let onModule = { Id = "f"; Adjacent = ["a"]; State = FlipFlopModuleState On }
    let offModule = { onModule with State = FlipFlopModuleState Off }
    test <@ Module.receivePulse "x" High onModule = ([], onModule) @>
    test <@ Module.receivePulse "x" High offModule = ([], offModule) @>

[<Fact>]
let ``flip flop module toggles state when receiving a low pulse`` () =
    let onModule = { Id = "f"; Adjacent = ["a"]; State = FlipFlopModuleState On }
    let offModule = { onModule with State = FlipFlopModuleState Off }
    test <@ Module.receivePulse "x" Low offModule = ([(High, "a")], onModule) @>
    test <@ offModule |> Module.receivePulse "x" Low |> snd |> Module.receivePulse "x" Low = ([(Low, "a")], offModule) @>

[<Fact>]
let ``conjunction module sends low pulse when all memory is high`` () =
    let ``module`` = { Id = "c"; Adjacent = ["a"; "b"]; State = ConjunctionModuleState (Map [("x", Low); ("b", High)]) }
    test <@ Module.receivePulse "x" High ``module`` = ([(Low, "a"); (Low, "b")], { ``module`` with State = ConjunctionModuleState (Map [("x", High); ("b", High)]) }) @>

[<Fact>]
let ``conjunction module sends high pulse when not all memory is high`` () =
    let ``module`` = { Id = "c"; Adjacent = ["a"; "b"]; State = ConjunctionModuleState (Map [("x", High); ("b", High)]) }
    test <@ Module.receivePulse "x" Low ``module`` = ([(High, "a"); (High, "b")], { ``module`` with State = ConjunctionModuleState (Map [("x", Low); ("b", High)]) }) @>

[<Fact>]
let ``button module sends low pulse to its children`` () =
    let ``module`` = { Id = "b"; Adjacent = ["broadcastt"]; State = ButtonModuleState }
    test <@ Module.receivePulse "x" High ``module`` = ([(Low, "broadcastt")], ``module``) @>

[<Fact>]
let ``broadcast module passes its pulse to its children`` () =
    let ``module`` = { Id = "b"; Adjacent = ["a"]; State = BroadcasterModuleState }
    test <@ Module.receivePulse "x" High ``module`` = ([(High, "a")], ``module``) @>
    test <@ Module.receivePulse "x" Low ``module`` = ([(Low, "a")], ``module``) @>
