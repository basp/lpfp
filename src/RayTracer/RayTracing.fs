module RayTracing

open System.Numerics
open Raylib_CSharp
open Raylib_CSharp.Colors
open Raylib_CSharp.Images

[<Measure>] type px
[<Measure>] type world

/// The canvas represents a 2D medium of cells into which values can be set.
/// For a rendering, this will usually be the dimensions of a bitmap (i.e. an
/// 2D array of Vector3 values).
type Canvas = {
    /// The width of the canvas (in pixels).
    Width: int<px>
    /// The height of the canvas (in pixels).
    Height: int<px>
}

/// The viewport is a rectangle (in world coordinates) onto which camera rays
/// are projected.
type Viewport = {
    /// The width of the viewport (in world space).
    Width: float32<world>
    /// The height of the viewport (in world space).
    Height: float32<world>
}

/// These are the minimal settings to initialize a camera.
type CameraSettings = {
    /// The aspect ratio of the camera.
    AspectRatio: float32
    /// The height of the canvas (in pixels).
    CanvasHeight: int<px>
    /// The height of the viewport (in world space).
    ViewportHeight: float32<world>
    /// The focus distance (in world space).    
    FocusDistance: float32<world>
}

/// Contains the parameters required to send rays into the world and to
/// translate between canvas and world space. Especially, DU, DV and 00 are
/// essential in order to iterate over the ray targets (pixels). 
type Camera = {
    /// The aspect ratio of the camera.
    AspectRatio: float32
    /// The dimensions of the canvas (in pixels).
    Canvas: Canvas
    /// The dimensions of the viewport (in world space).
    Viewport: Viewport
    /// The position of the camera (in world space).
    Position: Vector3    
    /// The horizontal stride (in world space) from pixel center to pixel
    /// center.
    PixelDU: Vector3
    /// The vertical stride (in world space) from pixel center to pixel center.
    PixelDV: Vector3
    /// The center of the upper left pixel (in world space).
    Pixel00: Vector3
}

/// A combination of an origin and a direction in world space.
type Ray = {
    /// The origin (starting point) of the ray (in world space).
    Origin: Vector3
    /// The direction of the ray (in world space).
    Direction: Vector3
}

type Object =
    | Sphere of Vector3 * float32

/// A cached inverse transform. 
type Transform = {
    // The original transform matrix.
    Matrix: Matrix4x4
    // The cached inverse transform matrix.
    Inverse: Matrix4x4    
}

/// Used to decouple objects from material and transformations.
type Instance = {
    Object: Object
    Transform: Transform option
    // TODO:
    // Instances should probably also have a material.
    // Or we might consider another type such as a primitive.
}

type Intersection = {
    // The t value of the intersection. This is the distance t along the ray
    // where the intersection was found.
    T: float32
    // The point (in world space) where the intersection occurred.
    Point: Vector3
    // The normal of the surface at the intersection point.
    Normal: Vector3
    // TODO:
    // We probably want to store the material ont he intersection.    
}

// Returns some cached transform. If the matrix is degenerate it returns none. 
let initTransform m =
    match Matrix4x4.Invert(m) with
    | true, inv -> Some { Matrix = m; Inverse = inv }
    | _ -> None

/// Initializes a new canvas based on an aspect ratio and canvas height.
let initCanvas aspectRatio canvasHeight =
    let canvasWidth =
        let width = float32 canvasHeight * aspectRatio
        match width |> int with
        | x when x < 1 -> 1
        | x -> x
        |> LanguagePrimitives.Int32WithMeasure<px>
    { Canvas.Width = canvasWidth
      Height = canvasHeight }    

/// Initializes a new viewport based on a canvas and viewport height.
let initViewport (canvas: Canvas) (viewportHeight: float32<world>) =
    let viewportWidth =
        // The aspect ratio we defined earlier is the *ideal* aspect ratio but
        // due to the integer conversion we want to re-calculate the actual
        // aspect ratio in order to give the viewport the correct dimensions.
        let ar = float32 canvas.Width / float32 canvas.Height
        viewportHeight * ar
    { Viewport.Width = viewportWidth
      Height = viewportHeight }
    
/// Initializes a new camera based on given camera settings. This sets up a
/// camera that uses a left-handed coordinate system.
let initCamera (settings: CameraSettings) =
    // The canvas represents the film of the camera in pixel units.
    let canvas = initCanvas settings.AspectRatio settings.CanvasHeight
    
    // The viewport represents the dimensions of the camera view in world units.
    let viewport = initViewport canvas settings.ViewportHeight
    
    // Viewport U and V are vectors representing the horizontal and vertical
    // dimensions of the canvas in world space. These are used to calculate the
    // pixel stride vectors DU and DV. Note that the Y coordinate is negated
    // here since we want the stride to go down but the y-axis to point up.
    let viewportU = Vector3(float32 viewport.Width, 0f, 0f)
    let viewportV = Vector3(0f, float32 -viewport.Height, 0f)
    
    // Pixel DU and DV represent respectively the horizontal and vertical
    // stride (in world space) between pixel centers. 
    let pixelDU = viewportU / (float32 canvas.Width)
    let pixelDV = viewportV / (float32 canvas.Height)
    
    // We will set the camera at the origin for now.
    let cameraPosition = Vector3.Zero
    
    // The world space coordinates of the upper left corner of the viewport.
    // We will use this as an offset point to calculate the center of the
    // pixel (0, 0) coordinate in world space.
    let viewportUpperLeft =
        cameraPosition +
        Vector3(0f, 0f, float32 settings.FocusDistance) -
        viewportU / 2f -
        viewportV / 2f
        
    // The center of the pixel (in world coordinates) of the pixel at canvas
    // coordinates (0, 0).
    let pixel00 = viewportUpperLeft + 0.5f * (pixelDU + pixelDV)
    
    // With everything calculated we are finally able to return a camera record.    
    { Camera.AspectRatio = settings.AspectRatio
      Canvas = canvas
      Viewport = viewport
      Position = cameraPosition
      PixelDU = pixelDU
      PixelDV = pixelDV
      Pixel00 = pixel00 }

/// Generates a ray for given pixel coordinates and camera.
let generateRay (x: int<px>) (y: int<px>) camera =
    // Calculate the center of the pixel specified by x and y in the arguments.
    // This will result in a pixel coordinate mapped to world space coordinates.
    let pixelCenter =
        camera.Pixel00 +
        (float32 x * camera.PixelDU) +
        (float32 y * camera.PixelDV)
    // Calculate the direction based on the pixel center (in world space) and
    // the camera position (in world space).
    let rayDirection = pixelCenter - camera.Position
    { Origin = camera.Position
      Direction = rayDirection }
    
/// Generates a sequence of rays for every pixel on the camera.
let rays camera =
    seq {
        for j in [0..(int camera.Canvas.Height - 1)] do
            for i in [0..(int camera.Canvas.Width - 1)] do
                let x = i * 1<px>
                let y = j * 1<px>
                camera |> generateRay x y                
    }
    
/// Provides a background color for a ray that doesn't intersect anything.
let backgroundColor ray =
    let unitDirection = Vector3.Normalize(ray.Direction)
    let a = 0.5f * (unitDirection.Y + 1f)
    (1f - a) * Vector3.One + a * Vector3(0.5f, 0.7f, 1f)
    
/// Normalizes a color vector to a Raylib color.
let normalizeColor (v: Vector3) =
    Vector4(v, 1f)
    |> Color.FromNormalized 

let intersectSphere center radius ray =
    let solve a b c =
        let D = b * b - 4f * a * c
        [(+); (-)]
        |> List.map (fun f -> (f -b (sqrt D)) / 2f / a)
    let oc = center - ray.Origin
    let a = Vector3.Dot(ray.Direction, ray.Direction)
    let b = -2f * Vector3.Dot(ray.Direction, oc)
    let c = Vector3.Dot(oc, oc) - radius * radius
    solve a b c |> List.sort   

let intersect (instance: Instance) ray =
    // Quadratic solver by [Dmitry Soshnikov](https://www.fssnip.net/38/title/Wicked-way-to-solve-quadratic-equation-using-list-of-operators).
    match instance.Object with
    | Sphere(center, radius) ->
        ray |> intersectSphere center radius
    
/// Basic camera usage example.
let example1 () =
    let camera =
        let settings =
          { CameraSettings.AspectRatio = 16f / 9f
            CanvasHeight = 200<px>
            ViewportHeight = 2f<world>
            FocusDistance = 1f<world> }
        initCamera settings
    let w = int camera.Canvas.Width
    let h = int camera.Canvas.Height
    let image = Image.GenColor(w, h, Color.Black)
    for j in [0..(h - 1)] do
        for i in [0..(w - 1)] do
            let x = i * 1<px>
            let y = i * 1<px>
            let color =
                camera
                |> generateRay x y
                |> backgroundColor
                |> normalizeColor
            image.DrawPixel(i, j, color)    
    image

/// Just a bit more terse way to setup a camera.
let example2 () =
    let camera =
        { CameraSettings.AspectRatio = 16f / 9f
          CanvasHeight = 200<px>
          ViewportHeight = 2f<world>
          FocusDistance = 1f<world> }
        |> initCamera
    camera
    
/// Test intersecting an actual sphere with a ray.
let example3 () =
    let obj =
        { Object = Sphere(Vector3.Zero, 1f)
          Transform = initTransform Matrix4x4.Identity } 
    let ray =
        { Origin = Vector3(0f, 0f, -5f)
          Direction = Vector3(0f, 0f, 1f) }
    let intersectSphere = intersect obj
    intersectSphere ray