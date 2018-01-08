open System
open System.IO

let x = File.ReadAllLines "22-input.txt"
x.[0].Length
x.Length

//25 = 12 :: 1 :: 12

let inputGrid = 
    let grid = 
        File.ReadAllLines "22-input.txt"
        |> Array.map (fun x -> x.ToCharArray())
    let length = grid.Length
    grid
    |> Array.indexed
    |> Array.collect (fun (r, cols) ->
        cols 
        |> Array.indexed
        |> Array.choose (fun (c, x) ->
            match x with
            | '#' -> Some ((r - (length/2), c - (length/2)), ())
            | _ -> None
        )
    )
    |> Map.ofArray

let sampleGrid = 
    [ (0, -1), ()
      (-1, 1), ()
    ]
    |> Map.ofList

let directions = 
    [ (-1, 0) // up
      ( 0, 1) // right
      ( 1, 0) // down
      ( 0,-1) // left
    ]

let (++) (x,y) (a,b) = (x + a, y + b)

let burst (grid, pos, dir, infections) = 
    let isCurrentNodeInfected = grid |> Map.containsKey pos

    let dirOffset, newGrid, infections = 
        if isCurrentNodeInfected then // turn right and remove infection
            1, grid |> Map.remove pos, infections
        else // clean -> // turn left and infect
            3, grid |> Map.add pos (), infections + 1
    
    let dir' = (dir + dirOffset) % directions.Length
    let posOffset = directions.[dir']
    let pos' = pos ++ posOffset
    newGrid, pos', dir', infections
    
[ 1 .. 10000 ]
|> List.fold (fun state _ -> burst state) (sampleGrid, (0,0), 0, 0)

let solution1 = 
    [ 1 .. 10000 ]
    |> List.fold (fun state _ -> burst state) (inputGrid, (0,0), 0, 0)

// Part 2

type NodeState = 
    | Clean | Weakened | Infected | Flagged

let modifyState = function
    | Clean -> Weakened
    | Weakened -> Infected
    | Infected -> Flagged
    | Flagged -> Clean

let burst2 (grid, pos, dir, infections) = 
    let currentNode = 
        grid
        |> Map.tryFind pos
        |> Option.defaultValue Clean

    let dirOffset =
        match currentNode with
        | Clean -> 3 // turn left
        | Weakened -> 0 // no turn
        | Infected -> 1 // turn right
        | Flagged -> 2 // reverse direction

    let newState = modifyState currentNode
    let grid' = grid |> Map.add pos newState
    let infections' = if newState = Infected then infections + 1 else infections
    let dir' = (dir + dirOffset) % directions.Length
    let posOffset = directions.[dir']
    let pos' = pos ++ posOffset
    grid', pos', dir', infections'

let sampleGrid2 = sampleGrid |> Map.map (fun _ _ -> Infected)

[ 1 .. 10000000 ]
|> List.fold (fun state _ -> burst2 state) (sampleGrid2, (0,0), 0, 0)

let inputGrid2 = inputGrid |> Map.map (fun _ _ -> Infected)

let solution2 = 
    [ 1 .. 10000000 ]
    |> List.fold (fun state _ -> burst2 state) (inputGrid2, (0,0), 0, 0)
