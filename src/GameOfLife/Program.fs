open Raylib_CSharp
open Raylib_CSharp.Colors
open Raylib_CSharp.Rendering
open Raylib_CSharp.Windowing

[<Struct>]
type Size = Size of (int * int)

[<Struct>]
type Offset = Offset of (int * int)

[<Struct>]
type Position = { Row: int; Column: int }     

type Generation = bool array2d

let seed alive (generation: Generation) =
    for pos in alive do
        generation[pos.Row, pos.Column] <- true
    generation

let initGeneration size alive : Generation =
    let (Size (height, width)) = size
    Array2D.create height width false
    |> seed alive

let neighborOffsets =
    [| Offset(-1, -1)
       Offset(-1, 0)
       Offset(-1, 1)
       Offset(0, -1)
       Offset(0, 1)
       Offset(1, -1)
       Offset(1, 0)
       Offset(1, 1)
    |]

let neighbors pos =
    neighborOffsets
    |> Array.map (fun (Offset (rowOffset, colOffset)) ->
        { Row = rowOffset + pos.Row
          Column = colOffset + pos.Column })

let calculateLiveNeighbors pos (generation: Generation) =
    pos
    |> neighbors
    |> Array.map (fun pos -> generation[pos.Row, pos.Column])
    |> Array.where (fun alive -> alive = true)
    |> Array.length

let update (current: Generation) : Generation =
    let next = Array2D.copy current
    let maxRow = Array2D.length1 next - 2
    let maxCol = Array2D.length2 next - 2
    for row in [1..maxRow] do
        for col in [1..maxCol] do
            let pos = { Row = row; Column = col }
            let liveNeighbors = current |> calculateLiveNeighbors pos
            let alive = current[row, col]
            next[row, col] <-
                match (alive, liveNeighbors) with
                | true, _ when liveNeighbors < 2 -> false
                | true, _ when liveNeighbors > 3 -> false
                | true, 2 -> true
                | true, 3 -> true
                | false, 3 -> true
                | _ -> false
    next
    
module Program =
    let blinker row col = 
        [
            { Row = row - 1; Column = col }
            { Row = row + 0; Column = col }
            { Row = row + 1; Column = col }
        ]
    
    let [<EntryPoint>] main _ =
        let gridSize = Size (7, 7)
        let mutable current =
            blinker 3 3
            |> initGeneration gridSize
        let windowSize = Size (240, 240)
        Window.Init (240, 240, "Game of Life")
        Time.SetTargetFPS 60        
        let mutable frameCounter = 0L        
        while (not <| Window.ShouldClose ()) do
            frameCounter <- frameCounter + 1L
            current <-
                if frameCounter % 120L = 0 then update current
                else current            
            Graphics.BeginDrawing ()
            do
                Graphics.ClearBackground Color.RayWhite
                Graphics.DrawFPS(10, 10)
                let (Size (rows, columns)) = gridSize
                let (Size (width, height)) = windowSize
                let dx = width / (columns + 1)
                let dy = height / (rows + 1)
                let radius = float32 (min dx dy) / 2f
                for row in [1..(rows - 2)] do
                    for col in [1..(columns - 2)] do
                        if current[row, col] then
                            let centerX = dx + col * dx
                            let centerY = dy + row * dy
                            Graphics.DrawCircle(
                                centerX,
                                centerY,
                                radius,
                                Color.SkyBlue)
            Graphics.EndDrawing ()            
        0