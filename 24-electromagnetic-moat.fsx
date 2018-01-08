open System
open System.IO

type Component = int * int
type Bridge = Component list

let sampleComponents =
    [
    0, 2
    2, 2
    2, 3
    3, 4
    3, 5
    0, 1
    10,1
    9,10
    ]

let parseComponent (x: string) =
    let temp = x.Split('/')
    int temp.[0], int temp.[1]

let inputComponents =
    File.ReadAllLines "24-input.txt"
    |> Seq.map parseComponent
    |> Seq.toList

let getCounterPart x (a,b) =
    if x = a
    then b
    else a

let solve1 components =
    let componentMap =
        components
        |> List.collect (fun (x,y) -> [ x, (x,y); y, (x,y) ])
        |> List.groupBy fst
        |> List.map (fun (key, xs) -> key, xs |> List.map snd |> List.distinct)
        |> Map.ofList

    let rec buildBridge (bridge: Component list) port : Component list list = 
        let possibleExtensionsComponents =
            componentMap
            |> Map.tryFind port
            |> Option.defaultValue []
            |> List.filter (fun x -> bridge |> List.contains x |> not)
        // printfn "exts: %A for %A" possibleExtensionsComponents bridge

        match possibleExtensionsComponents with
        | [] -> [ bridge ]
        | _ ->
            possibleExtensionsComponents
            |> List.collect (fun ext ->
                buildBridge (ext :: bridge) (getCounterPart port ext)
            )

    componentMap
    |> Map.find 0
    |> List.collect (fun startComponent ->
        buildBridge [ startComponent ] (getCounterPart 0 startComponent)
    )

let calcStrength = List.sumBy (fun (x,y) -> x + y)

let sampleSolution1 =
    solve1 sampleComponents
    |> List.map calcStrength
    |> List.max

let solution1 =
    solve1 inputComponents
    |> List.map calcStrength
    |> List.max

// Part 2
let findLongest =
    List.groupBy List.length
    >> List.maxBy fst
    >> snd
    >> List.map calcStrength
    >> List.max

let sampleSolution2 =
    solve1 sampleComponents
    |> findLongest

let solution2 =
    solve1 inputComponents
    |> findLongest