namespace GameOfLife

module Conway =
    
    /// Just to make sure we do not get confused by the row-column indexing.
    [<Struct>]
    type Position = { Row: int; Column: int }
    
    type IGrid<'a> =
        abstract member Rows: int
        abstract member Columns: int
        abstract member Item: Position -> 'a option with get, set
        abstract member Array: 'a array2d
    
    type Grid<'a>(arr: 'a array2d) =
        let rows = Array2D.length1 arr
        let cols = Array2D.length2 arr
        
        interface IGrid<'a> with
            member _.Array = arr
            member _.Rows = rows
            member _.Columns = cols
            member _.Item
                with get pos =
                    if pos.Row < 0 || pos.Row >= rows then None
                    else if pos.Column < 0 || pos.Column >= cols then None
                    else arr[pos.Row, pos.Column] |> Some
                and set pos value =
                    match value with
                    | Some a ->
                        if pos.Row < 0 || pos.Row >= rows then ()
                        else if pos.Column < 0 || pos.Column >= cols then ()
                        else arr[pos.Row, pos.Column] <- a
                    | None -> ()
    
    type WrapGrid<'a>(arr: 'a array2d) =
        let rows = Array2D.length1 arr
        let cols = Array2D.length2 arr
        
        let wrap v vMin vMax =
            if v < vMin then vMax + (v % vMax)
            else (v % vMax)
        let wrapRow row = wrap row 0 rows
        let wrapCol col = wrap col 0 cols

        interface IGrid<'a> with
            member _.Array = arr
            member _.Rows = rows
            member _.Columns = cols
            member _.Item
                with get pos =
                    let row' = wrapRow pos.Row
                    let col' = wrapCol pos.Column
                    Some arr[row', col']
                and set pos value =
                    match value with
                    | Some a ->
                        let row' = wrapRow pos.Row
                        let col' = wrapCol pos.Column
                        arr[row', col'] <- a
                    | None -> ()
    
    type GridFactory<'a> = 'a array2d -> IGrid<'a>
    
    /// This is just an alias for a bool array2d wrapper.
    type Generation = IGrid<bool>
        
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
    let calculateAliveNeighbors pos (gen: Generation) =
        pos
        |> neighbors
        |> Array.map (fun pos -> gen[pos])
        |> Array.where (fun opt -> opt |> Option.defaultValue false)
        |> Array.length

    /// Given a generation, this produces a new generation following
    /// the rules of Conway's algorithm.
    /// 
    /// https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life
    let update (factory: GridFactory<bool>) (generation: Generation) =
        generation.Array
        // First we map to the number of live neighbors.
        |> Array2D.mapi (fun row col _ ->
            let pos = { Row = row; Column = col }
            generation
            |> calculateAliveNeighbors pos)
        // Then we can execute Conway's rules easily.
        |> Array2D.mapi (fun row col aliveNeighbors ->
            let pos = { Row = row; Column = col }
            let areWeAlive =
                generation[pos]
                |> Option.defaultValue false    
            match (areWeAlive, aliveNeighbors) with
            // Less than two alive neighbors, underpopulation.
            | true, n when n < 2 -> false
            // More than three alive neighbors, overpopulation.
            | true, n when n > 3 -> false
            // Two alive neighbors is great but not quite
            // enough to bring a dead cell back...
            | true, 2 -> true
            // For that, we need at least 3 alive neighbors.
            | _, 3 -> true
            // No chance for survival.
            | _ -> false)
        |> factory
        
    /// Given 2D dimensions and a list of alive positions this will generate the
    /// first generation.
    let seed (rows, cols) (factory: GridFactory<bool>) alive : Generation =
        let arr =
            Array2D.create rows cols false
            |> factory
        alive |> Array.iter (fun pos -> arr[pos] <- Some true)
        arr
       