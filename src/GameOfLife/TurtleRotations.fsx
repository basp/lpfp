#r "nuget: Raylib-CSharp"

open System
open System.Collections.Generic
open System.Numerics
open Raylib_CSharp
open Raylib_CSharp.Camera.Cam2D
open Raylib_CSharp.Colors
open Raylib_CSharp.Rendering
open Raylib_CSharp.Windowing

open FSharp.Data.UnitSystems.SI.UnitSymbols

type [<Measure>] px;

type Turtle =
    { Position: Vector2
      Direction: Vector2 }

let drawLine (a: Vector2) (b: Vector2) =
    let x0 = int <| round a.X
    let y0 = int <| round a.Y
    let x1 = int <| round b.X
    let y1 = int <| round b.Y
    Graphics.DrawLine(x0, y0, x1, y1, Color.White)

let drawTurtle t =
    let scale = 10f
    let d = scale * (Vector2.Normalize(t.Direction))
    let m = Matrix3x2.CreateRotation(0.5f * MathF.PI)
    let u = Vector2.Transform(d, m)
    let u = MathF.Tan(0.45f) * u
    let a = t.Position - u
    let b = t.Position + d
    let c = t.Position + u
    drawLine a b
    drawLine c b
    
let turtle1 =
    { Position = Vector2.Zero
      Direction = Vector2(2f, 0f) }

let turtle2 =
    { Position = Vector2(-30f, 10f)
      Direction = Vector2(-4f, 2f) }

let turtles = [ turtle1; turtle2 ]

let test1 () =
    let (width, height) = 640, 320
    let halfWidth = float32 width / 2f
    let halfHeight = float32 height / 2f    
    let camera =
        let offset = Vector2(halfWidth, halfHeight)
        let target = Vector2.Zero
        Camera2D(offset, target, 0f, 1f)
          
    Window.Init(width, height, "Particles")
    Time.SetTargetFPS(60)
        
    while (not <| Window.ShouldClose()) do
        // Update
        let dt = Time.GetFrameTime()
                
        // Draw
        Graphics.BeginDrawing()
        do
            Graphics.ClearBackground(Color.Black)
            Graphics.BeginMode2D(camera)
            do
                for turtle in turtles do
                    drawTurtle turtle
            Graphics.EndMode2D()
        Graphics.EndDrawing()
        
    Window.Close()
    
test1 ()