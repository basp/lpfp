namespace GameOfLife

open System.Text.RegularExpressions
open GameOfLife.Conway
open Raylib_CSharp
open Raylib_CSharp.Colors
open Raylib_CSharp.Rendering
open Raylib_CSharp.Windowing
    
module Program =
    let blinker pos =
        [|
           { Row = pos.Row - 1; Column = pos.Column }
           { Row = pos.Row; Column = pos.Column }
           { Row = pos.Row + 1; Column = pos.Column }
        |]
        
    let glider pos =
        [|
            { Row = pos.Row - 1; Column = pos.Column - 1 }
            { Row = pos.Row; Column = pos.Column }
            { Row = pos.Row; Column = pos.Column + 1 }
            { Row = pos.Row + 1; Column = pos.Column - 1 }
            { Row = pos.Row + 1; Column = pos.Column }            
        |]
    
    let [<EntryPoint>] main _ =
        let pos = { Row = 2; Column = 2 }                   
        
        let rows, columns = (25, 25)
        
        let batch1 =
            [| glider pos
               glider { pos with Row = 3 }
               glider { pos with Column = 6 }
               glider { pos with Column = 3 }
               glider { pos with Row = 5 }
            |]
            |> Array.concat
            
        let batch2 =
            [| glider { pos with Row = 8 }
               glider { pos with Column = -5 }
               glider { pos with Row = -7 }
            |]
            |> Array.concat
        
        let gliders =
            [batch1; batch2]
            |> Array.concat
        
        let mutable current =
            batch2
            |> seed (rows, columns)
            
        let windowWidth, windowHeight = (440, 440)
        
        Window.Init(windowWidth, windowHeight, "Conway")        
        
        Time.SetTargetFPS 60                
        
        let dx = float32 windowWidth / float32 columns
        let dy = float32 windowHeight / float32 rows
        
        let mutable frameCounter = 0L
        let mutable generation = 0
        
        while (not <| Window.ShouldClose ()) do
            frameCounter <- frameCounter + 1L

            if frameCounter % 10L = 0 then
                current <- update current
                generation <- generation + 1
            else
                ()           
            
            Graphics.BeginDrawing ()
            do
                Graphics.ClearBackground Color.RayWhite
                for row in [0..(current.Rows - 1)] do
                    for col in [0..(current.Columns - 1)] do
                        let pos = { Row = row; Column = col }
                        if current[pos] then
                            let px = int <| round (float32 col * dx)
                            let py = int <| round (float32 row * dy)
                            Graphics.DrawRectangle(
                                px + 1,
                                py + 1,
                                int (dx - 2f),
                                int (dy - 2f),
                                Color.SkyBlue)
                Graphics.DrawFPS(10, 10)
                Graphics.DrawText(
                    $"Generation {generation}",
                    10,
                    40,
                    16,
                    Color.Black);
            Graphics.EndDrawing ()            
        0