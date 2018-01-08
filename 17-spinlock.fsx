open System
open System.Collections.Generic


let insert pos x (xs: 'a list) =
    xs.[0..pos] @ [ x ] @ xs.[pos+1..]

insert 0 42 [ 1 .. 10 ]
insert 9 42 [ 1 .. 10 ]

let spin stepSize currentPosition i buffer =
    let pos = (currentPosition + stepSize) % (List.length buffer)
    insert pos i buffer, pos + 1

spin 3 0 1 [0]
spin 3 1 2 [0;1]
spin 3 1 3 [0;2;1]
spin 3 2 4 [0;2;3;1]
spin 3 2 5 [0;2;4;3;1]
spin 3 1 6 [0;5;2;4;3;1]

let solution1 = 
    [ 1 .. 2017 ]
    |> List.fold (fun (buffer,pos) i -> spin 301 pos i buffer) ([0], 0)
    |> fun (buffer,pos) ->
        buffer.[pos+1]

let spin2 stepSize iterations =
    // i is also the length of the buffer
    let rec loop currentPosition i afterZero =
        if i < iterations then
            let pos = (currentPosition + stepSize) % i
            loop (pos+1) (i+1) (if pos = 0 then i else afterZero)
        else
            afterZero    
    loop 0 1 -1

spin2 301 50000000