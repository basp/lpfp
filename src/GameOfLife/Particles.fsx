#r "nuget: Raylib-CSharp"

open System
open System.Numerics
open Raylib_CSharp
open Raylib_CSharp.Camera.Cam2D
open Raylib_CSharp.Colors
open Raylib_CSharp.Rendering
open Raylib_CSharp.Windowing

type Range<'a> = {
    Min: 'a
    Max: 'a
}

type EmitterConfig = {
    Age: Range<float32>    
}

type Particle = {
    // TTL: float32
    // Age: float32
    // ShouldDeactivate: Particle -> bool
    mutable Pos: Vector2
    mutable Vel: Vector2
    mutable Acc: Vector2
}

type Emitter = {
    Particles: Particle list
}

type Rectangle = {
    Position: Vector2
    Width: float32
    Height: float32
}

let test2 () =
    let rng = Random()
    let (width, height) = 640, 320
    let camera =
        let halfWidth = float32 width / 2f
        let halfHeight = float32 height / 2f
        let offset = Vector2(halfWidth, halfHeight)
        let target = Vector2.Zero
        Camera2D(offset, target, 0f, 1f)
    let particles =
        [0..4]
        |> List.map (fun _ ->
            let x = -320f + rng.NextSingle() * 640f
            let yOffset = rng.NextSingle() * 100f
            { Pos = Vector2(x, float32 height / -2f - yOffset)
              Vel = Vector2.Zero
              Acc = Vector2(0f, 9.8f) })
            
    Window.Init(width, height, "Particles")
    Time.SetTargetFPS(60)
    
    while (not <| Window.ShouldClose()) do
        // Update
        let dt = Time.GetFrameTime()
        let damping = 0.6f
        for p in particles do
            p.Vel <- p.Vel + p.Acc * dt
            p.Pos <- p.Pos + p.Vel
            if MathF.Abs(p.Pos.Y) > 150f then
                p.Pos.Y <- 150f * (float32 <| MathF.Sign(p.Pos.Y))
                p.Vel <- -p.Vel * damping
        
        // Draw
        Graphics.BeginDrawing()
        do
            Graphics.ClearBackground(Color.Black)
            Graphics.BeginMode2D(camera)
            do
                // Graphics.DrawCircle(0, 0, 10f, Color.Red)
                // Graphics.DrawCircle(-320, 0, 10f, Color.Blue)
                // Graphics.DrawCircle(319, 0, 10f, Color.Blue)
                for p in particles do
                    let cx = int <| round p.Pos.X
                    let cy = int <| round p.Pos.Y
                    Graphics.DrawCircle(cx, cy, 10f, Color.SkyBlue)
            Graphics.EndMode2D()
        Graphics.EndDrawing()
    
    Window.Close()
    
let test1 () =
    let (width, height) = 640, 320

    let particles = [
        { Pos = Vector2(0f, float32 height / -2f)
          Vel = Vector2.Zero
          Acc = Vector2(0f, 9.8f) }
    ]
    
    let camera =
        let halfWidth = float32 width / 2f
        let halfHeight = float32 height / 2f
        let offset = Vector2(halfWidth, halfHeight)
        let target = Vector2.Zero
        Camera2D(offset, target, 0f, 1f)
        
    Window.Init(width, height, "Particles")
    Time.SetTargetFPS(60)
    
    while (not <| Window.ShouldClose()) do
        // Update
        let dt = Time.GetFrameTime()
        let damping = 0.6f
        for p in particles do
            p.Vel <- p.Vel + p.Acc * dt
            p.Pos <- p.Pos + p.Vel
            if MathF.Abs(p.Pos.Y) > 150f then
                p.Pos.Y <- 150f * (float32 <| MathF.Sign(p.Pos.Y))
                p.Vel <- -p.Vel * damping
        
        // Draw
        Graphics.BeginDrawing()
        do
            Graphics.ClearBackground(Color.Black)
            Graphics.BeginMode2D(camera)
            do
                // Graphics.DrawCircle(0, 0, 10f, Color.Red)
                // Graphics.DrawCircle(-320, 0, 10f, Color.Blue)
                // Graphics.DrawCircle(319, 0, 10f, Color.Blue)
                for p in particles do
                    let cx = int <| round p.Pos.X
                    let cy = int <| round p.Pos.Y
                    Graphics.DrawCircle(cx, cy, 10f, Color.SkyBlue)
            Graphics.EndMode2D()
        Graphics.EndDrawing()
    
    Window.Close()
    
test2 ()