using System.Numerics;
using System.Runtime.Intrinsics.X86;
using Raylib_CSharp.Camera.Cam2D;

namespace RandomStuff;

class GameOfLife
{
    private readonly int rows;
    private readonly int columns;

    private bool[][] current;
    private bool[][] next;
    
    record struct Position(int Row, int Column);

    public GameOfLife(int rows, int columns)
    {
        this.rows = rows;
        this.columns = columns;
        
        this.current = Enumerable
            .Range(0, rows + 2)
            .Select(_ => new bool[columns + 2])
            .ToArray();
        
        this.next = Enumerable
            .Range(0, rows + 2)
            .Select(_ => new bool[columns + 2])
            .ToArray();
    }

    public bool this[int row, int column]
    {
        get => this.current[row + 1][column + 1];
        set => this.current[row + 1][column + 1] = value;
    }

    public void Update()
    {
        for (var j = 1; j <= this.columns; j++)
        {
            for (var i = 1; i <= this.columns; i++)
            {
                var aliveNeighbors = this.CalculateAliveNeighbors(i, j);

                if (this.current[j][i])
                {
                    
                }
            }
        }
    }

    private int CalculateAliveNeighbors(int i, int j) =>
        GameOfLife.GetNeighborPositions(i, j)
            .Select(pos => this.current[pos.Row][pos.Column])
            .Count(v => v);
    
    private static Position[] GetNeighborPositions(int i, int j) =>
        new[]
        {
            new Position(-1, -1),
            new Position(-1, 0),
            new Position(-1, 1),
            new Position(0, 1),
            new Position(1, 1),
            new Position(1, 0),
            new Position(1, -1),
            new Position(0, -1),
        }
        .Select(offset =>
        {
            var row = j + offset.Row;
            var column = i + offset.Column;
            return new Position(row, column);
        })
        .ToArray();
}

internal static class Program
{
    public static void Main(string[] args)
    {
    }
}