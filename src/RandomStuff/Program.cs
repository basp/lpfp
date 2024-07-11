using Raylib_CSharp.Camera.Cam2D;

namespace RandomStuff;

using System.Numerics;
using DefaultEcs;
using DefaultEcs.System;
using DefaultEcs.Threading;

using Raylib_CSharp;
using Raylib_CSharp.Colors;
using Raylib_CSharp.Rendering;
using Raylib_CSharp.Windowing;

internal class Example01()
{
    private const int ScreenWidth = 800;
    private const int ScreenHeight = 450;

    private struct Position
    {
        public Vector2 Value;
    }

    private struct Velocity
    {
        public Vector2 Value;
    }

    private struct DrawInfo
    {
        public float Radius;
        
        public Color Color;

        public Vector2 Destination;
    }

    [With(typeof(Velocity))]
    private class VelocitySystem(World world, IParallelRunner runner) 
        : AEntitySetSystem<float>(world, runner)
    {
        private const float CollisionDamping = 0.8f;
        
        protected override void Update(float deltaTime, in Entity entity)
        {
            ref var velocity = ref entity.Get<Velocity>();
            ref var position = ref entity.Get<Position>();

            velocity.Value.Y += 9f;
            
            var offset = velocity.Value * deltaTime;

            position.Value.X += offset.X;
            position.Value.Y += offset.Y;

            var halfBoundsSize = new Vector2(10f, 10f);
            //
            // if (MathF.Abs(position.Value.X) > halfBoundsSize.X)
            // {
            //     position.Value.X = halfBoundsSize.X * MathF.Sign(position.Value.X);
            //     velocity.Value.X *= -1f;
            // }
            //
            if (MathF.Abs(position.Value.Y) > Example01.ScreenHeight / 2f - halfBoundsSize.Y)
            {
                position.Value.Y = (Example01.ScreenHeight / 2f - halfBoundsSize.Y) * MathF.Sign(position.Value.Y);
                velocity.Value.Y *= -1f * VelocitySystem.CollisionDamping;
            }
        }
    }

    private class DrawSystem(World world, IParallelRunner runner)
        : AComponentSystem<float, Position>(world, runner)
    {
        protected override void Update(
            float elapsedTime, 
            Span<Position> components)
        {
            foreach (var c in components)
            {
                var centerX = (int)MathF.Round(c.Value.X);
                var centerY = (int)MathF.Round(c.Value.Y);
                Graphics.DrawCircle(centerX, centerY, 10f, Color.SkyBlue);
            }
        }
    }
    
    public void Run()
    {
        IParallelRunner runner = new DefaultParallelRunner(Environment.ProcessorCount);
        
        var world = new World();
        var sphere = world.CreateEntity();
        sphere.Set(new Position
        {
            Value = Vector2.Zero,
        });
        sphere.Set(new Velocity
        {
            Value = new Vector2(0f, 10f),
        });

        var system = new SequentialSystem<float>(
            new VelocitySystem(world, runner),
            new DrawSystem(world, runner));
        
        Window.Init(
            Example01.ScreenWidth,
            Example01.ScreenHeight,
            "Example 1");

        var cameraOffset = new Vector2(
            Example01.ScreenWidth / 2f,
            Example01.ScreenHeight / 2f);
        
        var camera = new Camera2D(
            cameraOffset, 
            Vector2.Zero, 
            0f, 
            1f);
        
        while (!Window.ShouldClose())
        {
            var deltaTime = Time.GetFrameTime();

            // Update
            
            // Draw
            Graphics.BeginDrawing();
            Graphics.ClearBackground(Color.Black);
            Graphics.BeginMode2D(camera);

            system.Update(deltaTime);
            
            Graphics.EndMode2D();
            Graphics.EndDrawing();
        }
    }
}

internal static class Program
{
    public static void Main(string[] args)
    {
        var example = new Example01();
        example.Run();
    }
}