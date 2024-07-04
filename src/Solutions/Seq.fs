module Seq

let iterate f s =
    seq {
        let mutable current = s
        while true do
            yield current
            current <- f current
    }   
 
let cycle xs =
    seq {
        while true do
            for x in xs do
                yield x
    }
    
let repeat x =
    seq {
        while true do
            yield x
    }