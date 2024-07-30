namespace GameOfLife

open Conway
open GameOfLife.Conway
open Raylib_CSharp
open Raylib_CSharp.Colors
open Raylib_CSharp.Rendering
open Raylib_CSharp.Windowing
    
module Program =       
    let [<EntryPoint>] main _ =
        let factory : GridFactory<bool> =
            fun arr -> Grid<bool>(arr)

        let rows, columns = (200, 200)            
        let windowWidth, windowHeight = (800, 800)               
        
        let pos = { Row = 100; Column = 100 }                                          
        let initial = Examples.acorn pos                
        let mutable current =
            initial
            |> seed (rows, columns) factory
        
        Window.Init(windowWidth, windowHeight, "Conway")        
        
        Time.SetTargetFPS 60                
        
        let dx = float32 windowWidth / float32 columns
        let dy = float32 windowHeight / float32 rows
        
        let mutable frameCounter = 0L
        let mutable generation = 0
        let mutable alive = Array.length initial
        
        while (not <| Window.ShouldClose ()) do
            frameCounter <- frameCounter + 1L

            if (frameCounter % 2L = 0) && (alive > 0) then
                current <- update factory current
                generation <- generation + 1
            else
                ()           
            
            Graphics.BeginDrawing ()
            do
                alive <- 0
                Graphics.ClearBackground Color.RayWhite
                for row in [0..(current.Rows - 1)] do
                    for col in [0..(current.Columns - 1)] do
                        let pos = { Row = row; Column = col }
                        match current[pos] with
                        | Some a when a ->
                            alive <- alive + 1
                            let px = int <| round (float32 col * dx)
                            let py = int <| round (float32 row * dy)
                            Graphics.DrawRectangle(
                                px,
                                py,
                                int dx,
                                int dy,
                                Color.DarkBlue)
                            // Graphics.DrawRectangle(
                            //     px + 1,
                            //     py + 1,
                            //     int (dx - 2f),
                            //     int (dy - 2f),
                            //     Color.SkyBlue)
                        | _ -> ()
                Graphics.DrawFPS(10, 10)
                Graphics.DrawText(
                    $"Generation: {generation}",
                    10,
                    40,
                    24,
                    Color.Black)
                Graphics.DrawText(
                    $"Alive: {alive}",
                    10,
                    70,
                    24,
                    Color.Black)
            Graphics.EndDrawing ()            
        0