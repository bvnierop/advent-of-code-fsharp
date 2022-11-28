namespace AdventOfCode.Solutions.Year2021

open System

module Day01 =
    let solve1 () =
        let values = System.IO.File.ReadAllLines("input/2021/01.in") |> Array.map Int32.Parse |> Array.toSeq
        Seq.zip values (Seq.skip 1 values)
        |> Seq.where (fun (a, b) -> a < b)
        |> Seq.length
        
    let solve2 () =
        let values = System.IO.File.ReadAllLines("input/2021/01.in") |> Array.map Int32.Parse |> Array.toSeq
        Seq.zip values (Seq.skip 3 values)
        |> Seq.filter (fun (a, b) -> a < b)
        |> Seq.length
