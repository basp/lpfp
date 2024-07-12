namespace GameOfLife

open Raylib_CSharp
open Raylib_CSharp.Colors
open Raylib_CSharp.Rendering
open Raylib_CSharp.Windowing
    
open Conway
    
module Program =
    let [<EntryPoint>] main _ =
        let initial =
            [| { Row = 1; Column = 2 }
               { Row = 2; Column = 2 }
               { Row = 3; Column = 2 }
            |]
            
        let mutable current = initial |> seed (5, 5)
        printfn $"%A{current.Array}"
        
        current <- update current
        printfn $"%A{current.Array}"
        
        current <- update current
        printfn $"%A{current.Array}"
        
        // Window.Init (440, 440, "Conway")
        // Time.SetTargetFPS 60                
        // let mutable frameCounter = 0L        
        // while (not <| Window.ShouldClose ()) do
        //     frameCounter <- frameCounter + 1L
        //     // Update
        //     
        //     Graphics.BeginDrawing ()
        //     do
        //         Graphics.ClearBackground Color.RayWhite
        //         Graphics.DrawFPS(10, 10)
        //         // Draw
        //     
        //     Graphics.EndDrawing ()            
        0