open System

type Group = {
    GarbageCharCount: int
    Groups: Group list
}

let rec parseGarbage input cancelledCount =
    match input with
    | '!' :: _ :: rest -> parseGarbage rest cancelledCount
    | '>' :: rest -> printfn "%A" cancelledCount; rest, cancelledCount
    | _   :: rest -> parseGarbage rest (cancelledCount + 1)
    | [] -> failwith "Missing >"

let rec parseGroup input groups cancelledCount =
    match input with
    | '<' :: rest -> 
        let rest', count = parseGarbage rest 0
        parseGroup rest' groups (cancelledCount + count)
    | '{' :: rest ->
        let group, rest' = parseGroup rest [] 0
        parseGroup rest' (group :: groups) cancelledCount
    | '}' :: rest -> { Groups = groups; GarbageCharCount = cancelledCount }, rest
    | ',' :: rest -> parseGroup rest groups cancelledCount // skip

let parse input =
    match input with
    | '{' :: rest -> 
        let groups,rest = parseGroup rest [] 0
        if rest <> [] then
            failwith "didn't consume the whole input"
        groups        
    | x -> failwith "Expecting '{'"

let parseString = Seq.toList >> parse

let score (group: Group) =
    let rec score' level g =
        level + (g.Groups |> List.sumBy (score' (level+1)))
    score' 1 group

let rec sumCancelled (group: Group) =
    group.GarbageCharCount + (group.Groups |> List.sumBy sumCancelled)

parseString "{}" |> score
parseString "{{{}}}" |> score
parseString "{{},{}}" |> score
parseString "{{{},{},{{}}}}" |> score
parseString "{<{},{},{{}}>}" |> score
parseString "{<a>,<a>,<a>,<a>}" |> score
parseString "{{<a>},{<a>},{<a>},{<a>}}" |> score
parseString "{{<!>},{<!>},{<!>},{<a>}}" |> score
parseString "{{<ab>},{<ab>},{<ab>},{<ab>}}" |> score
parseString "{{<!!>},{<!!>},{<!!>},{<!!>}}" |> score
parseString "{{<a!>},{<a!>},{<a!>},{<ab>}}" |> score

parseString "{<>}" |> sumCancelled // 0 characters.
parseString "{<random characters>}" |> sumCancelled // 17 characters.
parseString "{<<<<>}" |> sumCancelled // 3 characters.
parseString "{<{!>}>}" |> sumCancelled // 2 characters.
parseString "{<!!>}" |> sumCancelled // 0 characters.
parseString "{<!!!>>}" |> sumCancelled // 0 characters.
parseString "{<{o\"i!a,<{i<a>}" |> sumCancelled // 10 characters.
parseString "{<{o\"i!a,<{i<a>{}}" |> sumCancelled // 10 characters.


let puzzleInput = IO.File.ReadAllText "9-input.txt"
let puzzleGroup = parseString puzzleInput
let part1 = score puzzleGroup 
let part2 = sumCancelled puzzleGroup
