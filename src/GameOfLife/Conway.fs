namespace GameOfLife

module Conway =
    
    /// Just to make sure we do not get confused by the row-column indexing.
    [<Struct>]
    type Position = { Row: int; Column: int }

    /// Allows for wrap-around indexing of a 2D array.
    type WrapArray2D<'a>(arr: 'a array2d) =
        // The number of rows in the 2D array.
        let rows = Array2D.length1 arr
        
        // The number of columns in the 2D array.
        let cols = Array2D.length2 arr

        // Wraps a value so that whatever v is, it will always
        // wrap around to be inside vMin and vMax.
        let wrap v vMin vMax =
            if v < vMin then vMax + (v % vMax)
            else (v % vMax)
            //
            // let v' = if v < vMin then vMax + (v % vMax) else v
            // v' % vMax    
        
        // Curried version of wrap so that we only need to specify
        // the row later on.
        let wrapRow row = wrap row 0 rows
        
        // Curried version of wrap so that we only need to specify
        // the column later on.
        let wrapCol col = wrap col 0 cols
        
        /// This is a reference to the underlying 2D array instance.
        /// Mostly useful for debugging and printing purposes and
        /// not intended to be used outside of these use cases.
        member _.Array = arr
        
        /// The number of rows in the 2D grid.
        member _.Rows = rows
        
        /// The number of columns in the 2D grid.
        member _.Columns = cols
        
        /// Gets or sets the cell at given position.
        member _.Item
            with get pos =
                let row' = wrapRow pos.Row
                let col' = wrapCol pos.Column
                arr[row', col']
            and set pos value =
                let row' = wrapRow pos.Row
                let col' = wrapCol pos.Column
                arr[row', col'] <- value

    /// This is just an alias for a bool array2d wrapper.
    type Generation = WrapArray2D<bool>
        
    /// Returns a list of neighboring positions based on the given position.
    let neighbors pos =
        // This list represents the offsets of all eight cells
        // surrounding the given position.
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
            
        // Generates a new position based on the given position and offset.
        let offsetPosition offset =
            { Row = offset.Row + pos.Row
              Column = offset.Column + pos.Column }
            
        // Generate the neighbor positions based on the offsets and the
        // mapping function defined above.
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

    /// Given a generation, this produces a new generation following
    /// the rules of Conway's algorithm.
    /// 
    /// https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life
    let update (generation: Generation) =
        generation.Array
        // First we map to the number of live neighbors.
        |> Array2D.mapi (fun row col _ ->
            let pos = { Row = row; Column = col }
            generation
            |> calculateAliveNeighbors pos)
        // Then we can execute Conway's rules easily.
        |> Array2D.mapi (fun row col aliveNeighbors ->
            let pos = { Row = row; Column = col }
            let areWeAlive = generation[pos]
            match (areWeAlive, aliveNeighbors) with
            // Less than two alive neighbors, underpopulation.
            | true, _ when aliveNeighbors < 2 -> false
            // More than three alive neighbors, overpopulation.
            | true, _ when aliveNeighbors > 3 -> false
            // Two alive neighbors is excellent but not quite
            // enough to bring a dead cell back.
            | true, 2 -> true
            // For that, we need at least 3 alive neighbors.
            | _, 3 -> true
            // No chance for survival.
            | _ -> false)
        |> Generation
        
    /// Given 2D dimensions and a list of alive positions this will generate the
    /// first generation.
    let seed (rows, cols) alive : Generation =
        let arr =
            Array2D.create rows cols false
            |> WrapArray2D
        alive
        |> Array.iter (fun pos -> arr[pos] <- true)
        arr
       