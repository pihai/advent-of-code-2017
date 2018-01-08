open System

let offsets = function
    | "n"  ->  0,  1
    | "ne" ->  1,  0
    | "se" ->  1, -1
    | "s"  ->  0, -1
    | "sw" -> -1,  0
    | "nw" -> -1,  1
    | _ -> failwith "invalid move"

let sumTuple (x1, y1) (x2, y2) = x1 + x2, y1 + y2

let distance (x1,y1) (x2, y2) =
    [ abs (x1-x2)
      abs (y1-y2)
      abs ((0-x1-y1) - (0-x2-y2))
    ]
    |> List.max

[ "ne"; "ne"; "ne" ]
|> List.map offsets
|> List.fold sumTuple (0, 0)

let input = (IO.File.ReadAllText "11-input.txt").Split(',')

input
|> Seq.map offsets
|> Seq.fold sumTuple (0, 0)
|> distance (0,0)

// Part 2
input
|> Seq.map offsets
|> Seq.scan sumTuple (0, 0)
|> Seq.map (distance (0,0))
|> Seq.max
