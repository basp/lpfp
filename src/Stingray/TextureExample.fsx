#r "nuget: Raylib-CSharp"

open System
open System.Numerics

open Raylib_CSharp
open Raylib_CSharp.Camera.Cam2D
open Raylib_CSharp.Colors
open Raylib_CSharp.Rendering
open Raylib_CSharp.Textures
open Raylib_CSharp.Windowing

let sampleGaussian mean stddev (random: Random) =
    let x1 = 1f - random.NextSingle()
    let x2 = 1f - random.NextSingle()
    let y1 =
        MathF.Sqrt(-2f * MathF.Log(x1)) *
        MathF.Cos(2f * MathF.PI * x2)
    y1 * stddev + mean

let example () =
    let random = Random()
    
    let screenWidth, screenHeight = (640, 240)
    let camera =
        let halfWidth = float32 screenWidth / 2f
        let halfHeight = float32 screenHeight / 2f
        let offset = Vector2(halfWidth, halfHeight)
        Camera2D(offset, Vector2.Zero, 0f, 1f)
        
    Window.Init(screenWidth, screenHeight, "PF")
    Time.SetTargetFPS(60)
    
    let circle = RenderTexture2D.Load(21, 21)
    Graphics.BeginTextureMode(circle)
    Graphics.DrawCircle(10, 10, 8f, new Color(0uy, 0uy, 0uy, 255uy))
    Graphics.DrawCircle(10, 10, 6f, new Color(127uy, 127uy, 127uy, 255uy))
    Graphics.EndTextureMode()
    
    let target = RenderTexture2D.Load(screenWidth, screenHeight)
    
    Graphics.BeginTextureMode(target)
    Graphics.ClearBackground(Color.RayWhite)
    Graphics.EndTextureMode()
    
    while (not <| Window.ShouldClose()) do
        Graphics.BeginTextureMode(target)
        Graphics.BeginMode2D(camera)
        do
            // let x =
            //     random
            //     |> sampleGaussian 0f 30f
            //     |> round
            //     |> int
            // let y =
            //     random
            //     |> sampleGaussian 0f 30f
            //     |> round
            //     |> int
            // let g =
            //     random
            //     |> sampleGaussian 60f 10f
            //     |> round
            //     |> byte
            // let b =
            //     random
            //     |> sampleGaussian 160f 10f
            //     |> round
            //     |> byte
            // Graphics.DrawCircle(x, y, 4f, Color(0uy, g, b, 200uy))
            Graphics.DrawTexture(circle.Texture, 0, 0, Color.White)
        Graphics.EndMode2D()
        Graphics.EndTextureMode()
        
        Graphics.BeginDrawing()
        Graphics.DrawTexture(target.Texture, 0, 0, Color.White)
        Graphics.EndDrawing()
    
    Window.Close()

example ()