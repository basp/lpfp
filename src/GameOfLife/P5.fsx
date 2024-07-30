#r "nuget: Raylib-CSharp"

open System
open System.Numerics
open Raylib_CSharp
open Raylib_CSharp.Camera.Cam2D
open Raylib_CSharp.Colors
open Raylib_CSharp.Rendering
open Raylib_CSharp.Windowing

type Particle = {
    Position: Vector2
    Acceleration: Vector2
    Velocity: Vector2
    Lifespan: float32
}

let rng = Random()

let random min max =
    let t = rng.NextSingle()
    min + t * (max - min)

let initParticle x y =
    let vx = random -1f 1f
    let vy = random -1f 0f
    { Position = Vector2(x, y)
      Acceleration = Vector2.Zero
      Velocity = Vector2(vx, vy)
      Lifespan = 1f }

let applyForce force particle =
    let acc' = particle.Acceleration + force
    { particle with Acceleration = acc' }

let show p =
    let color =
        Vector4(0.5f, 0.5f, 0.5f, p.Lifespan)
        |> Color.FromNormalized
    let x = int <| round p.Position.X
    let y = int <| round p.Position.Y
    Graphics.DrawCircle(x, y, 8f, color)

let update p =
    let vel' = p.Velocity + p.Acceleration
    let pos' = p.Position + vel'
    let lifespan' = p.Lifespan - 0.03f
    let acc' = Vector2.Zero
    { Position = pos'
      Velocity = vel'
      Acceleration = acc'
      Lifespan = lifespan' }

let isAlive p = p.Lifespan < 0f

let test1 () =
    let (width, height) = 640, 320
    let camera =
        let halfWidth = float32 width / 2f
        let halfHeight = float32 height / 2f
        let offset = Vector2(halfWidth, halfHeight)
        let target = Vector2.Zero
        Camera2D(offset, target, 0f, 1f)
    Window.Init(width, height, "P5")
    Time.SetTargetFPS(60)
    while (not <| Window.ShouldClose()) do
        let dt = Time.GetFrameTime()
        Graphics.BeginDrawing()
        do
            Graphics.ClearBackground(Color.RayWhite)
            Graphics.BeginMode2D(camera)
            do
                Graphics.DrawCircle(0, 0, 16f, Color.SkyBlue)
                ()
            Graphics.EndMode2D()
        Graphics.EndDrawing()
    Window.Close()
    
test1 ()