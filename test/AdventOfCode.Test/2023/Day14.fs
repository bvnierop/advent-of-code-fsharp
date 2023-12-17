module AdventOfCode.Test._2023.Day14

open System
open Xunit
open Swensen.Unquote
open AdventOfCode.Solutions._2023.Day14

module Grid2D =
    let testGrid =
        [ "#...."
          "....."
          "....."
          "....."
          "....." ] |> Grid2D.make

    [<Fact>]
    let ``search up`` () =
        let predicate = (fun c -> c = '#')
        test <@ Grid2D.tryFindUp predicate (Grid2D.Row 4) (Grid2D.Column 0) testGrid = Some (Grid2D.Row 0, Grid2D.Column 0) @>
        test <@ Grid2D.tryFindUp predicate (Grid2D.Row 0) (Grid2D.Column 0) testGrid = Some (Grid2D.Row 0, Grid2D.Column 0) @>
        test <@ Grid2D.tryFindUp predicate (Grid2D.Row 4) (Grid2D.Column 1) testGrid = None @>
