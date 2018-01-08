type Grid = char[,]
open System

let flipV (xs: 'a[,]) =
    let height, width = xs.GetLength 0, xs.GetLength 1
    let result = Array2D.zeroCreate height width
    for i in 0 .. height - 1 do
        for j in 0 .. width - 1 do
            result.[i,j] <- xs.[i, width - j - 1]
    result

let flipH (xs: 'a[,]) = 
    let height, width = xs.GetLength 0, xs.GetLength 1
    let result = Array2D.zeroCreate height width
    for i in 0 .. height - 1 do
        for j in 0 .. width - 1 do
            result.[i, j] <- xs.[height - i - 1, j]
    result

let rotate (xs: 'a[,]) = 
    let height, width = xs.GetLength 0, xs.GetLength 1
    let result = Array2D.zeroCreate height width
    for i in 0 .. height - 1 do
        for j in 0 .. width - 1 do
            result.[i, j] <- xs.[height - j - 1, i]
    result

// let a = Array2D.init 3 3 (fun i j -> (i * 3) + j)
// flipV a
// flipH a
// rotate a
// (rotate >> rotate) a
// (rotate >> rotate >> rotate >> rotate) a = a

let parsePattern (pattern: string) = 
    pattern.Split('/')
    |> Array.map (fun x -> x.ToCharArray())
    |> array2D

let parseRule (text: string) =
    let parts = text.Split([|" => "|], StringSplitOptions.None)
    parsePattern (parts.[0]), parsePattern (parts.[1])

let allPatternsForRule (g: Grid) =
    let rotate1 = rotate g
    let rotate2 = rotate rotate1
    let rotate3 = rotate rotate2
    [ g; rotate1; rotate2; rotate3; flipH g; flipV g; flipV rotate1; flipV rotate2; flipV rotate3 ]

let chunk chunkSize (xs: 'a[,]) =
    let size = xs.GetLength 0
    let chunksIndices = [ 0 .. (size / chunkSize) - 1 ]
    chunksIndices
    |> List.map (fun r ->
        chunksIndices |> List.map (fun c ->
            let i,j = r * chunkSize, c * chunkSize
            xs.[i .. (i + chunkSize - 1), j .. (j + chunkSize - 1)]
        )
    )

let combine (xs: 'a[,] list list) =
    let chunkSize = xs.Head.Head.GetLength 0
    xs
    |> List.collect (fun row ->
        [ 0 .. chunkSize - 1 ]
        |> List.map (fun r ->
            row |> List.collect (fun chunk -> chunk.[r,0..] |> Array.toList)
        )
    )
    |> array2D

// let grid = Array2D.init 10 10 (fun i j -> (i * 10) + j)
// grid |> chunk 2 |> combine = grid

let enhance (grid: Grid) (rules: Map<Grid, Grid>) = 
    let size = grid.GetLength 0
    let chunkSize = 
        if size % 2 = 0 
        then 2
        else 3

    let chunks = chunk chunkSize grid
    chunks
    |> List.map (fun row ->
        row |> List.map (fun input ->
            match rules |> Map.tryFind input with
            | Some pattern -> pattern
            | None -> failwithf "No pattern found for %A" input
        )
    )
    |> combine

let intitalGrid = 
    [ ".#."
      "..#"
      "###"
    ]
    |> List.map (fun x -> x.ToCharArray())
    |> array2D

let sampleRules = 
    [ "../.# => ##./#../..."
      ".#./..#/### => #..#/..../..../#..#"
    ]
    |> List.map parseRule

let allSampleRules =
    sampleRules
    |> List.collect (fun (input, output) ->
        allPatternsForRule input |> List.map (fun x -> x, output)
    )
    |> Map.ofList

let countOn (g: Grid) = g |> Seq.cast<char> |> Seq.countBy id |> Seq.toList

let x = enhance intitalGrid allSampleRules
let y = enhance x allSampleRules

countOn y

let inputRules = 
    IO.File.ReadAllLines "21-input.txt"
    |> Seq.map parseRule
    |> Seq.collect (fun (input, output) ->
        allPatternsForRule input |> List.map (fun x -> x, output)
    )
    |> Seq.toList
    |> Map.ofSeq

inputRules |> Map.tryFind intitalGrid

inputRules |> Map.count

let solution1 = 
    [ 1 .. 5 ]
    |> List.fold (fun g _ ->
        enhance g inputRules
    ) intitalGrid
    |> countOn

let solution2 = 
    [ 1 .. 18 ]
    |> List.fold (fun g _ ->
        enhance g inputRules
    ) intitalGrid
    |> countOn



let a = enhance intitalGrid inputRules

countOn result

countOn intitalGrid

IO.File.ReadAllLines "21-input.txt"
|> Seq.map parseRule
|> Seq.filter (fun (i,_) -> countOn i = countOn intitalGrid)
|> Seq.toList
// |> Seq.collect (fun (input, output) ->
//     allPatternsForRule input |> List.map (fun x -> x, output)
// )
// |> Seq.toList
// |> Map.ofSeq