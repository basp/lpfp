let mutable current = Array2D.init 5 5 (fun _ _ -> false)

let mutable next = Array2D.init 5 5 (fun _ _ -> false)

let mutable state = (current, next)

let neighbors i j =
    [ (-1, -1)
      (0, -1)
      (1, -1)
      (1, 0)
      (1, 1)
      (0, 1)
      (-1, 1)
      (-1, 0) ]
    |> List.map (fun (oi, oj) -> (i + oi, j + oj))
    
let countLiveNeighbors i j (grid: bool array2d) =
    neighbors i j
    |> List.filter (fun (i, j) -> current[j, i])
    |> List.length
    
let updateGeneration (grid: bool array2d) =
    for 
    ()