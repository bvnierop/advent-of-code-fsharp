module AdventOfCode.Test._2023.Day19

open System
open Xunit
open Swensen.Unquote
open AdventOfCode.Solutions._2023.Day19
open AdventOfCode.Lib

[<Fact>]
let ``parse workflow`` () =
    let workflow = "px{a<2006:qkq,m>2090:A,x}"
    let expected = {
        Id = "px"
        Steps = [
            { Condition = Predicate (A, SmallerThan 2006); Target = "qkq" }
            { Condition = Predicate (M, GreaterThan 2090); Target = "A" }
            { Condition = Always; Target = "x" }
        ]
    }
    test <@ Workflow.parse workflow = expected @>

[<Fact>]
let ``parse part`` () =
    let part = "{x=787,m=2655,a=1222,s=2876}"
    let expected = {
        X = 787
        M = 2655
        A = 1222
        S = 2876
    }
    test <@ Part.parse part = expected @>

[<Fact>]
let ``parse input`` () =
    let input = """gd{a>3333:R,R}
hdj{m>838:A,pv}

{x=787,m=2655,a=1222,s=2876}
{x=1679,m=44,a=2067,s=496}"""
    let workflows, parts = parse input
    test <@ List.length workflows = 2 @>
    test <@ List.length parts = 2 @>

[<Theory>]
[<InlineData("{x=787,m=2655,a=2005,s=2876}", "qkq")>]
[<InlineData("{x=787,m=2091,a=2006,s=2876}", "A")>]
[<InlineData("{x=787,m=2090,a=2006,s=2876}", "rfg")>]
let ``execute workflow`` partStr expected =
   let workflow = "px{a<2006:qkq,m>2090:A,rfg}" |> Workflow.parse
   let part = partStr |> Part.parse

   test <@ Workflow.execute part workflow = expected @>

let ``execute many workflows test data`` () : obj[] seq = seq {
    yield [| "{x=10,m=10,a=10,s=10}"; Accept |]
    yield [| "{x=75,m=10,a=10,s=10}"; Reject |]
}

[<Theory>]
[<MemberData(nameof(``execute many workflows test data``))>]
let ``execute many workflows`` partStr expected =
    let workflows =
        [ "in{aa}"
          "aa{x<100:bb,A}"
          "bb{x<50:A,R}" ]
        |> List.map Workflow.parse

    let part = partStr |> Part.parse
    test <@ Workflow.executeMany part workflows = expected @>

let ``execute a workflow on a range test data`` () : obj[] seq =
    let parts = { XR = Range.create 0 4000
                  MR = Range.create 0 4000
                  AR = Range.create 0 4000
                  SR = Range.create 0 4000 }
    seq {
        yield [| "in{aa}"; [("aa", parts)] |]
        yield [| "in{bb}"; [("bb", parts)] |]
        yield [| "in{x<100:aa,bb}"; [("bb", { parts with XR = Range.create 100 4000 })
                                     ("aa", { parts with XR = Range.create 0 100 })] |]
        yield [| "in{x>100:aa,bb}"; [("bb", { parts with XR = Range.create 0 101 })
                                     ("aa", { parts with XR = Range.create 101 4000 })] |]
    }

[<Theory>]
[<MemberData(nameof(``execute a workflow on a range test data``))>]
let ``execute a workflow on a range`` workflowStr expected =
    let workflow = workflowStr |> Workflow.parse
    let parts = { XR = Range.create 0 4000
                  MR = Range.create 0 4000
                  AR = Range.create 0 4000
                  SR = Range.create 0 4000 }

    test <@ PartRange.executeWorkflow parts workflow = expected @>


[<Fact>]
let ``part 2`` () =
    let workflows = """px{a<2006:qkq,m>2090:A,rfg}
pv{a>1716:R,A}
lnx{m>1548:A,A}
rfg{s<537:gd,x>2440:R,A}
qs{s>3448:A,lnx}
qkq{x<1416:A,crn}
crn{x>2662:A,R}
in{s<1351:px,qqz}
qqz{s>2770:qs,m<1801:hdj,R}
gd{a>3333:R,R}
hdj{m>838:A,pv}

"""
    test <@ solve2 workflows = 167409079868000L @>
                            // 256000000000000L
