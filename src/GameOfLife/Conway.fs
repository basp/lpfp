namespace GameOfLife

module Conway =
    [<Struct>]
    type Position = { Row: int; Column: int }

    /// Wraps a <see cref="Array2D"/> to allow for wrap-around indexing.
    type WrapArray2D<'a>(arr: 'a array2d) =
        let rows = Array2D.length1 arr
        
        let cols = Array2D.length2 arr

        let wrap v vMin vMax =
            let v' = if v < vMin then vMax + (v % vMax) else v
            v' % vMax    
        
        let wrapRow row = wrap row 0 rows
        
        let wrapCol col = wrap col 0 cols
        
        member _.Array = arr
        
        member _.Rows = rows
        
        member _.Columns = cols
        
        member _.Item
            with get pos =
                let row' = wrapRow pos.Row
                let col' = wrapCol pos.Column
                arr[row', col']
            and set pos value =
                let row' = wrapRow pos.Row
                let col' = wrapCol pos.Column
                arr[row', col'] <- value

    type Generation = WrapArray2D<bool>
        
    /// Returns a list of neighboring positions based on the given position.
    let neighbors pos =
        let neighborOffsets =
            [| { Row = -1; Column = -1 }
               { Row = -1; Column = 0 }
               { Row = -1; Column = 1 }
               { Row = 0; Column = -1 }
               { Row = 0; Column = 1 }
               { Row = 1; Column = -1 }
               { Row = 1; Column = 0 }
               { Row = 1; Column = 1 }
            |]
        let offsetPosition offset =
            { Row = offset.Row + pos.Row
              Column = offset.Column + pos.Column }        
        neighborOffsets
        |> Array.map offsetPosition

    /// Calculates the number of alive neighbors for given position and
    /// generation.
    let calculateAliveNeighbors pos (gen: WrapArray2D<bool>) =
        pos
        |> neighbors
        |> Array.map (fun pos -> gen[pos])
        |> Array.where id
        |> Array.length

    /// Updates a generation to produce a new generation.
    let update (generation: Generation) =
        generation.Array
        |> Array2D.mapi (fun row col _ ->
            let pos = { Row = row; Column = col }
            generation
            |> calculateAliveNeighbors pos)
        |> Array2D.mapi (fun row col n ->
            let pos = { Row = row; Column = col }
            let alive = generation[pos]
            match (alive, n) with
            // Less than two alive neighbors, underpopulation.
            | true, _ when n < 2 -> false
            // More than three alive neighbors, overpopulation.
            | true, _ when n > 3 -> false
            // Two or three neighbors is the sweet spot.
            | true, 2 -> true
            | _, 3 -> true
            // No chance for survival.
            | _ -> false)
        |> Generation
        
    let seed (rows, cols) alive : Generation =
        let arr =
            Array2D.create rows cols false
            |> WrapArray2D
        alive
        |> Array.iter (fun pos -> arr[pos] <- true)
        arr