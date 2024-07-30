namespace GameOfLife

open Conway

module Examples =
    let blinker pos =
        [|
           { Row = pos.Row - 1; Column = pos.Column }
           { Row = pos.Row; Column = pos.Column }
           { Row = pos.Row + 1; Column = pos.Column }
        |]
        
    let glider pos =
        [|
            { Row = pos.Row - 1; Column = pos.Column - 1 }
            { Row = pos.Row; Column = pos.Column }
            { Row = pos.Row; Column = pos.Column + 1 }
            { Row = pos.Row + 1; Column = pos.Column - 1 }
            { Row = pos.Row + 1; Column = pos.Column }            
        |]

    let batch1 pos =
        [| glider pos
           glider { pos with Row = 3 }
           glider { pos with Column = 6 }
           glider { pos with Column = 3 }
           glider { pos with Row = 5 }
        |]
        |> Array.concat
        
    let batch2 pos =
        [| glider { pos with Row = 8 }
           glider { pos with Column = -5 }
           glider { pos with Row = -7 }
        |]
        |> Array.concat
       
    let gliderGun pos =
        [| pos
           { pos with Column = pos.Column + 1 }
           { pos with Row = pos.Row + 1 }
           { Row = pos.Row + 1; Column = pos.Column + 1 }
           
           { pos with Column = pos.Column + 10 }
           { Row = pos.Row + 1; Column = pos.Column + 10 }
           { Row = pos.Row + 2; Column = pos.Column + 10 }
           
           { Row = pos.Row - 1; Column = pos.Column + 11 }
           { Row = pos.Row + 3; Column = pos.Column + 11 }
           
           { Row = pos.Row - 2; Column = pos.Column + 12 }
           { Row = pos.Row - 2; Column = pos.Column + 13 }

           { Row = pos.Row + 4; Column = pos.Column + 12 }
           { Row = pos.Row + 4; Column = pos.Column + 13 }
           
           { Row = pos.Row + 1; Column = pos.Column + 14 }
           
           { Row = pos.Row - 1; Column = pos.Column + 15 }
           { Row = pos.Row + 3; Column = pos.Column + 15 }
           
           { Row = pos.Row + 0; Column = pos.Column + 16 }
           { Row = pos.Row + 1; Column = pos.Column + 16 }
           { Row = pos.Row + 2; Column = pos.Column + 16 }
           
           { Row = pos.Row + 1; Column = pos.Column + 17 }
           
           { Row = pos.Row + 0; Column = pos.Column + 20 }
           { Row = pos.Row + 0; Column = pos.Column + 21 }
           { Row = pos.Row - 1; Column = pos.Column + 20 }
           { Row = pos.Row - 1; Column = pos.Column + 21 }
           { Row = pos.Row - 2; Column = pos.Column + 20 }
           { Row = pos.Row - 2; Column = pos.Column + 21 }
           
           { Row = pos.Row - 3; Column = pos.Column + 22 }
           { Row = pos.Row + 1; Column = pos.Column + 22 }
           
           { Row = pos.Row - 3; Column = pos.Column + 24 }
           { Row = pos.Row - 4; Column = pos.Column + 24 }
           
           { Row = pos.Row + 1; Column = pos.Column + 24 }
           { Row = pos.Row + 2; Column = pos.Column + 24 }
           
           { Row = pos.Row - 1; Column = pos.Column + 34 }
           { Row = pos.Row - 2; Column = pos.Column + 34 }
           { Row = pos.Row - 1; Column = pos.Column + 35 }
           { Row = pos.Row - 2; Column = pos.Column + 35 }
        |]
        
    let diehard pos =
        [| 
            pos
            { pos with Column = pos.Column + 1 }
            { Row = pos.Row + 1; Column = pos.Column + 1 }
            { Row = pos.Row + 1; Column = pos.Column + 5 }
            { Row = pos.Row + 1; Column = pos.Column + 6 }
            { Row = pos.Row + 1; Column = pos.Column + 7 }
            { Row = pos.Row - 1; Column = pos.Column + 6 }
        |] 

    let acorn pos =
        [| { Row = pos.Row + 1; Column = pos.Column + 2 }
           { Row = pos.Row + 2; Column = pos.Column + 4 }
           { Row = pos.Row + 3; Column = pos.Column + 1 }
           { Row = pos.Row + 3; Column = pos.Column + 2 }
           { Row = pos.Row + 3; Column = pos.Column + 5 }
           { Row = pos.Row + 3; Column = pos.Column + 6 }
           { Row = pos.Row + 3; Column = pos.Column + 7 }
        |]