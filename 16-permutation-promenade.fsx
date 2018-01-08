open System

let spin size (xs: 'a list) =
    let cutPoint = xs.Length - size
    xs.[cutPoint ..] @ xs.[0 .. cutPoint - 1]

spin 4 [ 'a'; 'b'; 'c'; 'd'; 'e' ]

let exchange (pos1, pos2) xs =
    xs
    |> List.mapi (fun i x ->
        if i = pos1 
        then xs.[pos2]
        elif i = pos2
        then xs.[pos1]
        else x
    )

let partner (a, b) xs =
    xs
    |> List.map (fun x ->
        if x = a 
        then b
        elif x = b
        then a
        else x
    )

let s = spin
let x = exchange
let p = partner

[ 'a'; 'b'; 'c'; 'd'; 'e' ]
|> s 1
|> x (3, 4)
|> p ('e', 'b')

let parseMove (move: string) =
    let moveType = move.[0]
    let args = move.[1..]
    let parsePair converter =
        let [| a; b |] = args.Split('/')
        converter a, converter b
    // printfn "%s" args
    match moveType with
    | 's' -> args |> int |> s
    | 'x' -> parsePair int |> x
    | 'p' -> parsePair (Seq.head) |> p

let moves =
    IO.File.ReadAllText("16-input.txt").Split(',')
    |> Array.map parseMove

let initial = ['a'..'p']
moves.Length

List.replicate 10 [1;2;3]

let solution1 = 
    moves
    |> Array.fold (fun state move -> move state) initial
    |> List.toArray
    |> String

let solution2 = 
    Seq.init 1000000000 (fun i -> moves.[i % moves.Length])
    |> Seq.fold (fun state move -> move state) initial
    |> List.toArray
    |> String