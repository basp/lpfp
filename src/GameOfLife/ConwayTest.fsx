[<Struct>]
type Position = { Row: int; Column: int }

type WrapArray2D<'a>(arr: 'a array2d) =
    let rows = Array2D.length1 arr
    
    let cols = Array2D.length2 arr

    let wrap v vMin vMax =
        let v' = if v < vMin then vMax + v else v
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

let calculateAliveNeighbors pos (gen: WrapArray2D<bool>) =
    pos
    |> neighbors
    |> Array.map (fun pos -> gen[pos])
    |> Array.where id
    |> Array.length

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
        | true, _ when n < 2 -> false
        | true, _ when n > 3 -> false
        | true, 2 -> true
        | _, 3 -> true
        | _ -> false)
    |> Generation
    
let current = Array2D.create 5 5 false |> Generation

current[{ Row = 1; Column = 2 }] <- true
current[{ Row = 2; Column = 2 }] <- true
current[{ Row = 3; Column = 2 }] <- true

let vMin = 0
let vMax = 5
let v = -1

let wrap v vMin vMax =
    let v' = if v < vMin then vMax + (v % vMax) else v
    v' % vMax
    
wrap -7 0 3