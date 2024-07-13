namespace GameOfLife

open Conway
open Raylib_CSharp
open Raylib_CSharp.Colors
open Raylib_CSharp.Rendering
open Raylib_CSharp.Windowing
    
module Program =       
    let [<EntryPoint>] main _ =
        let factory : GridFactory<bool> =
            fun arr -> Grid<bool>(arr)

        let rows, columns = (80, 80)            
        let windowWidth, windowHeight = (880, 880)               
        
        let pos = { Row = 10; Column = 5 }                                          
        let initial = Examples.gliderGun pos                
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

            if (frameCounter % 10L = 0) && (alive > 0) then
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
                                px + 1,
                                py + 1,
                                int (dx - 2f),
                                int (dy - 2f),
                                Color.SkyBlue)
                        | _ -> ()
                Graphics.DrawFPS(10, 10)
                Graphics.DrawText(
                    $"Generation {generation}",
                    10,
                    40,
                    24,
                    Color.Black);
            Graphics.EndDrawing ()            
        0