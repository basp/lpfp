#r "nuget: Raylib-CSharp"

open System.Numerics
open Raylib_CSharp
open Raylib_CSharp.Camera.Cam2D
open Raylib_CSharp.Colors
open Raylib_CSharp.Rendering
open Raylib_CSharp.Windowing

let initCamera width height =
    let halfWidth = float32 width / 2f
    let halfHeight = float32 height / 2f
    let offset = Vector2(halfWidth, halfHeight)
    Camera2D(offset, Vector2.Zero, 0f, 1f)
    
let test () =
    let viewportWidth, viewportHeight = (640, 640)
    let camera = initCamera viewportWidth viewportHeight
    let upperLeft = Vector2(
        float32 viewportWidth / -2f,
        float32 viewportHeight / -2f)
    let deltaU = Vector2(float32 viewportWidth / 16f, 0f)
    let deltaV = Vector2(0f, float32 viewportHeight / 16f)
    let origin = upperLeft + (deltaU / 2f) + (deltaV / 2f)
    Window.Init(viewportWidth, viewportHeight, "Test")
    Time.SetTargetFPS 60
    while (not <| Window.ShouldClose()) do
        Graphics.BeginDrawing()
        do
            Graphics.ClearBackground(Color.RayWhite)
            Graphics.BeginMode2D(camera)
            for row in [0..15] do
                for col in [0..15] do
                    let pos =
                        origin +
                        (float32 col * deltaU) +
                        (float32 row * deltaV)
                    let x = int <| round pos.X
                    let y = int <| round pos.Y
                    Graphics.DrawCircle(x, y, 4f, Color.Orange)
            Graphics.EndMode2D()
        Graphics.DrawFPS(5, 5)
        Graphics.EndDrawing()
    Window.Close()
    
test ()

