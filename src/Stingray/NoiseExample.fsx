#r "nuget: Raylib-CSharp"

open System
open System.Numerics
open Raylib_CSharp
open Raylib_CSharp.Colors
open Raylib_CSharp.Images
open Raylib_CSharp.Rendering
open Raylib_CSharp.Textures
open Raylib_CSharp.Windowing

module NoiseGen =
    let xNoiseGen = 1619
    let yNoiseGen = 31337
    let zNoiseGen = 6971
    let seedNoiseGen = 1013
    let shiftNoiseGen = 8

    let permutations () =
        let random = Random()
        Array.init 256 (fun _ -> random.Next(0, 255))
        
    let randomVectors =
        let perm = permutations ()
        Array.append perm perm
        
    let intValueNoise3D seed (x, y, z) =
        let mutable n = (
            xNoiseGen * x +
            yNoiseGen * y +
            zNoiseGen * z +
            seedNoiseGen * seed) &&& 0x7fffffff
        n <- (n >>> 13) ^^^ n
        (n * (n * n * 60493 + 19990303) + 1376312589) &&& 0x7fffffff

    let valueNoise3D seed pos =
        let n = intValueNoise3D seed pos |> float32
        (1f - n) / 1073741824f

let permutation () =
    let random = Random()
    Array.init 256 (fun _ -> random.Next(0, 255))
    
let p =
    let perm = permutation ()
    Array.append perm perm
    
let fade t = t * t * t * (t * (t * 6f - 15f) + 10f)

let lerp (t, a, b) = a + t * (b - a)

let grad (hash, x, y, z) : float32 =
    let h = hash &&& 15
    let u = if h < 8 then x else y
    let v =
        if h < 4 then y
        else
            if h = 12 || h = 14 then x
            else z
    (if h &&& 1 = 0 then u else -u) +
    (if h &&& 2 = 0 then v else -v)
        
let noise (x, y, z) =
    let floorInt (v: float32) = int <| floor v
    
    let X = floorInt x &&& 255
    let Y = floorInt y &&& 255
    let Z = floorInt z &&& 255
    
    let x = x - floor x
    let y = y - floor y
    let z = z - floor z
    
    let u = fade x
    let v = fade y
    let w = fade z
    
    let A = p[X] + Y
    let B = p[X+1] + Y
    let AA = p[A] + Z
    let BA = p[B] + Z
    let AB = p[A + 1] + Z
    let BB = p[B + 1] + Z
    
    lerp (w, lerp (v, lerp (u, grad (p[ AA ], x,     y,     z    ),
                               grad (p[ BA ], x-1f,  y,     z    )),
                      lerp (u, grad (p[ AB ], x,     y-1f,  z    ),
                               grad (p[ BB ], x-1f,  y-1f,  z    ))),
             lerp (v, lerp (u, grad (p[AA+1], x,     y,     z-1f ),
                               grad (p[BA+1], x-1f,  y,     z-1f )),
                      lerp (u, grad (p[AB+1], x,     y-1f,  z-1f ),
                               grad (p[BB+1], x-1f,  y-1f,  z-1f ))))
let example () =
    Window.Init(640, 240, "Noise")
    Time.SetTargetFPS(60)
    
    let image = RenderTexture2D.Load(640, 240)
    
    Graphics.BeginTextureMode(image)
    let mutable xOff = 0f    
    for x in [0..639] do
        let mutable yOff = 0f
        for y in [0..239] do
            let n = noise (xOff, yOff, 0f)
            let bright =
                RayMath.Remap(n, -1f, 1f, 0f, 255f)
                |> round
                |> byte
            let color = new Color(bright, bright, bright, 255uy)
            Graphics.DrawPixel(x, y, color)
            yOff <- yOff + 0.01f
        xOff <- xOff + 0.01f
    Graphics.EndTextureMode()        
    
    while (not <| Window.ShouldClose()) do
        Graphics.BeginDrawing()
        Graphics.ClearBackground(Color.Black)
        Graphics.DrawTexture(image.Texture, 0, 0, Color.White)
        Graphics.EndDrawing()
    Window.Close()

let example2 () =
    let smoothStep t = t * t * (3f - 2f * t)
    let random = Random()
    let kMaxTableSize = 256
    let kMaxTableSizeMask = kMaxTableSize - 1
    let r = Array.init (kMaxTableSize * kMaxTableSize) (fun _ -> random.NextSingle())    
    let eval (pos: Vector2) =
        let xi = MathF.Floor(pos.X)
        let yi = MathF.Floor(pos.Y)
        
        let tx = pos.X - xi
        let ty = pos.Y - yi
        
        let rx0 = (int xi) &&& kMaxTableSizeMask
        let rx1 = (rx0 + 1) &&& kMaxTableSizeMask
        let ry0 = (int yi) &&& kMaxTableSizeMask
        let ry1 = (ry0 + 1) &&& kMaxTableSizeMask
        
        let c00 = r[ry0 * kMaxTableSizeMask + rx0]
        let c10 = r[ry0 * kMaxTableSizeMask + rx1]
        let c01 = r[ry1 * kMaxTableSizeMask + rx0]
        let c11 = r[ry1 * kMaxTableSizeMask + rx1]
        
        let sx = smoothStep tx
        let sy = smoothStep ty
        
        let nx0 = lerp (sx, c00, c10)
        let nx1 = lerp (sx, c01, c11)
        
        lerp (sy, nx0, nx1)

    let image = Image.GenColor(640, 240, Color.Black)
    let frequency = 0.5f
    for j in [0 .. (240 - 1)] do
        for i in [0 .. (640 - 1)] do
            let pos = Vector2(float32 i, float32 j) * frequency
            let n = eval pos
            let color = Color.FromNormalized(Vector4(n, n, n, 1f))
            image.DrawPixel(i, j, color)
    image.Export(@"d:\temp\test.png")
    
let example3 () =
    let smoothStep t = t * t * (3f - 2f * t)
    let random = Random()
    let kMaxTableSize = 256
    let kMaxTableSizeMask = kMaxTableSize - 1
    let r = Array.init (kMaxTableSize * kMaxTableSize) (fun _ -> random.NextSingle())    
    let eval (pos: Vector2) =
        let xi = MathF.Floor(pos.X)
        let yi = MathF.Floor(pos.Y)
        
        let tx = pos.X - xi
        let ty = pos.Y - yi
        
        let rx0 = (int xi) &&& kMaxTableSizeMask
        let rx1 = (rx0 + 1) &&& kMaxTableSizeMask
        let ry0 = (int yi) &&& kMaxTableSizeMask
        let ry1 = (ry0 + 1) &&& kMaxTableSizeMask
        
        let c00 = r[ry0 * kMaxTableSizeMask + rx0]
        let c10 = r[ry0 * kMaxTableSizeMask + rx1]
        let c01 = r[ry1 * kMaxTableSizeMask + rx0]
        let c11 = r[ry1 * kMaxTableSizeMask + rx1]
        
        let sx = smoothStep tx
        let sy = smoothStep ty
        
        let nx0 = lerp (sx, c00, c10)
        let nx1 = lerp (sx, c01, c11)
        
        lerp (sy, nx0, nx1)

    let image = Image.GenColor(640, 240, Color.Black)
    let frequency = 0.5f
    for j in [0 .. (240 - 1)] do
        for i in [0 .. (640 - 1)] do
            let pos = Vector2(float32 i, float32 j) * frequency
            let n = eval pos
            let color = Color.FromNormalized(Vector4(n, n, n, 1f))
            image.DrawPixel(i, j, color)
    image.Export(@"d:\temp\test.png")