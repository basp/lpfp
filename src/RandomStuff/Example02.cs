using Raylib_CSharp;
using Raylib_CSharp.Colors;
using Raylib_CSharp.Rendering;
using Raylib_CSharp.Windowing;

namespace RandomStuff;

internal class Example02
{
    public void Run()
    {
        var current =
            Enumerable
                .Range(0, 66)
                .Select(_ => new bool[66])
                .ToArray();

        var next =
            Enumerable
                .Range(0, 66)
                .Select(_ => new bool[66])
                .ToArray();

        current[31][32] = true;
        current[32][32] = true;
        current[33][32] = true;

        int CalculateLiveNeighbours(int i, int j)
        {
            var numberOfAliveNeighbours = 0;
            foreach (var oj in new[] { -1, -1, -1, 0, 1, 1, 1, 0 })
            {
                foreach (var oi in new[] { -1, 0, 1, 1, 1, 0, -1, -1 })
                {
                    if (current[j + oj][i + oi])
                    {
                        numberOfAliveNeighbours++;
                    }
                }
            }

            return numberOfAliveNeighbours;
        }

        void UpdateGeneration()
        {
            for (var j = 1; j <= 64; j++)
            {
                for (var i = 1; i <= 64; i++)
                {
                    var numberOfLiveNeighbors =
                        CalculateLiveNeighbours(i, j);

                    // Cell is alive
                    if (current[j][i])
                    {
                        // Underpopulation
                        if (numberOfLiveNeighbors < 2)
                        {
                            next[j][i] = false;
                        }
                        // Overpopulation
                        else if (numberOfLiveNeighbors > 3)
                        {
                            next[j][i] = false;
                        }
                        else
                        {
                            next[j][i] = true;
                        }
                    }
                    // Cell is dead
                    else
                    {
                        if (numberOfLiveNeighbors == 3)
                        {
                            next[j][i] = true;
                        }
                    }
                }
            }
        }

        const int screenWidth = 800;
        const int screenHeight = 450;

        // var camera = new Camera2D(
        //     new Vector2(screenWidth / 2f, screenHeight / 2f),
        //     Vector2.Zero, 
        //     0f, 
        //     1f);

        Window.Init(screenWidth, screenHeight, "Example 2");

        Time.SetTargetFPS(60);

        long frameCount = 0;

        var radius = 2.5f;
        var offsetY = 65;
        var offsetX = 240;

        while (!Window.ShouldClose())
        {
            // var dt = Time.GetFrameTime();

            frameCount++;

            if (frameCount % 60 == 0)
            {
                UpdateGeneration();

                (current, next) = (next, current);
            }

            Graphics.BeginDrawing();
            {
                Graphics.ClearBackground(Color.RayWhite);

                for (var j = 1; j <= 64; j++)
                {
                    for (var i = 1; i <= 64; i++)
                    {
                        if (current[j][i])
                        {
                            var x = (i - 1) * 5 + offsetX;
                            var y = (j - 1) * 5 + offsetY;
                            Graphics.DrawCircle(x, y, radius, Color.SkyBlue);
                        }
                    }
                }

                // Graphics.BeginMode2D(camera);
                // {
                //     Graphics.DrawCircle(
                //         0, 
                //         0, 
                //         200f, 
                //         Color.SkyBlue);
                // }
                // Graphics.EndMode2D();
            }
            Graphics.EndDrawing();
        }
    }
}