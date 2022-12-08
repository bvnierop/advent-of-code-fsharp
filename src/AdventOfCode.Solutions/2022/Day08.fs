namespace AdventOfCode.Solutions._2022

open AdventOfCode.Lib.Solver
open System

module Day08 =
    [<AocSolver(2022, 8, Level = 1)>]
    let solve1 (input: string list) =
        let treeMap =
            input
            |> List.map (Seq.map Int32.parseChr)
            |> array2D

        let rows = Array2D.length1 treeMap
        let columns = Array2D.length2 treeMap

        let visible = Array2D.init rows columns
                                   (fun r c ->
                                        match (r, c) with
                                        | (0, _) -> true
                                        | (_, 0) -> true
                                        | (row, _) when row = rows - 1 -> true
                                        | (_, col) when col = columns - 1 -> true
                                        | _ -> false)

        let isVisible (row, col)  high =
            visible[row,col] || 
            treeMap[row,col] > high

        for row = 1 to rows - 2 do // Skip the edges
            let mutable high = treeMap[row,0]
            for left = 1 to columns - 2 do
                visible[row,left] <- isVisible (row, left) high
                high <- max high treeMap[row,left]
            high <- treeMap[row, columns - 1]
            for right = columns - 2 downto 1 do
                visible[row,right] <- isVisible (row, right) high
                high <- max high treeMap[row,right]

        for column = 1 to columns - 2 do
            let mutable high = treeMap[0,column]
            for top = 1 to rows - 2 do
                visible[top,column] <- isVisible (top, column) high
                high <- max high treeMap[top,column]
            high <- treeMap[rows - 1, column]
            for bottom = rows - 2 downto 1 do
                visible[bottom,column] <- isVisible (bottom, column) high
                high <- max high treeMap[bottom,column]

        visible
        |> Seq.cast
        |> Seq.countWhere id
        
    [<AocSolver(2022, 8, Level = 2)>]
    let solve2 (input: string list) =
        let treeMap =
            input
            |> List.map (Seq.map Int32.parseChr)
            |> array2D
            
        let rows = Array2D.length1 treeMap
        let columns = Array2D.length2 treeMap
            
        let scenicScore row col treeHeight =
            let mutable score = 1
            let mutable visible = 0
            let mutable blocked = false
            for r = row - 1 downto 0 do
                if treeMap[r,col] >= treeHeight && not blocked then visible <- visible + 1; blocked <- true
                if not blocked then visible <- visible + 1
            score <- score * visible
            visible <- 0
            blocked <- false
            for r = row + 1 to rows - 1 do
                if treeMap[r,col] >= treeHeight && not blocked then visible <- visible + 1; blocked <- true
                if not blocked then visible <- visible + 1
            score <- score * visible
            visible <- 0
            blocked <- false
            for c = col - 1 downto 0 do
                if treeMap[row,c] >= treeHeight && not blocked then visible <- visible + 1; blocked <- true
                if not blocked then visible <- visible + 1
            score <- score * visible
            visible <- 0
            blocked <- false
            for c = col + 1 to columns - 1 do
                if treeMap[row,c] >= treeHeight && not blocked then visible <- visible + 1; blocked <- true
                if not blocked then visible <- visible + 1
            score * visible
        
        treeMap
        |> Array2D.mapi scenicScore
        |> Seq.cast<int>
        |> Seq.max