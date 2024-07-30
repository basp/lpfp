using System.Numerics;
using System.Runtime.CompilerServices;
using Raylib_CSharp;
using Raylib_CSharp.Camera.Cam2D;
using Raylib_CSharp.Colors;
using Raylib_CSharp.Rendering;
using Raylib_CSharp.Textures;
using Raylib_CSharp.Windowing;

namespace RandomStuff;

internal static class Utils
{
    private static readonly Random rng = new Random();

    public static double SampleGaussian(float mean, float stddev)
    {
        var x1 = 1f - rng.NextSingle();
        var x2 = 1f - rng.NextSingle();
        var y1 = 
            MathF.Sqrt(-2f * MathF.Log(x1)) * 
            MathF.Cos(2f * MathF.PI * x2);
        return y1 * stddev + mean;
    }
    
    public static float RandomSingle(float min, float max)
    {
        var t = Utils.rng.NextSingle();
        return min + t * (max - min);
    }
}

internal class Particle
{
    public Particle(float x, float y)
    {
        this.Position = new Vector2(x, y);
        this.Acceleration = Vector2.Zero;
        this.Velocity = new Vector2(
            Utils.RandomSingle(-1.5f, 1.5f),
            Utils.RandomSingle(-2, 0));
        this.Lifespan = 255;
    }

    public Vector2 Position;

    public Vector2 Acceleration;

    public Vector2 Velocity;

    public int Lifespan;

    public bool IsDead => this.Lifespan < 0;
    
    public void Run()
    {
        var gravity = new Vector2(0, 0.05f);
        this.ApplyForce(gravity);
        this.Update();
        this.Show();
    }

    public void ApplyForce(Vector2 force)
    {
        this.Acceleration += force;
    }

    public void Update()
    {
        this.Velocity += this.Acceleration;
        this.Position += this.Velocity;
        this.Lifespan -= 2;
        this.Acceleration *= 0f;
    }

    public void Show()
    {
        var outline = new Color(0, 0, 0, (byte)this.Lifespan);
        var fill = new Color(127, 127, 127, (byte)this.Lifespan);
        var x = (int)MathF.Round(this.Position.X);
        var y = (int)MathF.Round(this.Position.Y);
        Graphics.DrawCircle(x, y, 8f, outline);
        Graphics.DrawCircle(x, y, 6f, fill);
    }
}

internal class Emitter
{
    private readonly Vector2 origin;
    
    private List<Particle> particles;
    
    public Emitter(float x, float y)
    {
        this.origin = new Vector2(x, y);
        this.particles = new List<Particle>();
    }

    public void AddParticle()
    {
        var particle = new Particle(this.origin.X, this.origin.Y);
        this.particles.Add(particle);
    }

    public void Run()
    {
        for (var i = this.particles.Count - 1; i >= 0; i--)
        {
            this.particles[i].Run();
            if (this.particles[i].IsDead)
            {
                this.particles.RemoveAt(i);
            }
        }
        
        // foreach (var p in this.particles)
        // {
        //     p.Run();
        // }
        //
        // this.particles = this.particles
        //     .Where(p => !p.IsDead)
        //     .ToList();
    }
}

internal static class Program
{
    public static void Main(string[] args)
    {
        var (width, height) = (640, 240);
        var halfWidth = width / 2f;
        var halfHeight = height / 2f;
        var offset = new Vector2(halfWidth, halfHeight);
        var camera = new Camera2D(offset, Vector2.Zero, 0f, 1f);

        var emitter = new Emitter(0, -20);
        
        Window.Init(width, height, "P5");
        Time.SetTargetFPS(60);
        
        while (!Window.ShouldClose())
        {
            Graphics.BeginDrawing();
            Graphics.ClearBackground(Color.RayWhite);
            Graphics.BeginMode2D((camera));
            emitter.Run();
            emitter.AddParticle();
            Graphics.EndMode2D();
            Graphics.EndDrawing();
        }
    }
}