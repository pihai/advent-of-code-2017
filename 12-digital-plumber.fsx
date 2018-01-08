open System

let input =
    IO.File.ReadAllLines "12-input.txt"
    |> Seq.map (fun line ->
        let parts = line.Split([|" <-> "|], StringSplitOptions.None)
        let prog = int parts.[0]
        let subProgs = 
            parts.[1].Split(',')
            |> Seq.map int
            |> Set.ofSeq
        prog, subProgs
    )
    |> Seq.toList

let connectSets sets =
    sets
    |> Set.map (fun set ->
        sets
        |> Set.filter ((<>) set)
        |> Seq.tryFind (not << Set.isEmpty << (Set.intersect set))
        |> Option.map (Set.union set)
        |> Option.defaultValue set
    )

let connectAllGroups sets =
    let rec loop prevSets =
        printfn "%d" (Set.count prevSets)
        let sets' = connectSets prevSets
        if sets' = prevSets then
            sets'
        else
            loop sets'
    loop sets            

let groups = 
    input
    |> List.map (fun (x,y) -> y.Add x)
    |> Set.ofList
    |> connectAllGroups

let group0Size = 
    groups
    |> Set.filter (Set.contains 0)
    |> Set.map (Set.count)

let groupCount = groups.Count