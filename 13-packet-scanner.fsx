open System
 
let isCaught (depth, range) = depth % ((range - 1) * 2) = 0 

let layers =
    IO.File.ReadAllLines "13-input.txt"
    |> Seq.map (fun line ->
        let parts = line.Split([|": "|], StringSplitOptions.None)
        int parts.[0], int parts.[1]
    )
    |> Seq.toList

let solve1 =
    layers
    |> List.filter isCaught
    |> List.sumBy (fun (depth, range) -> depth * range)

let solve2 =
    Seq.initInfinite id
    |> Seq.find (fun delay ->
        layers
        |> List.map (fun (depth, range) -> depth + delay, range)
        |> List.forall (isCaught >> not)
    )
