open System
open System.IO

let parseLine (line: string) =
    let digits = [| '0' .. '9' |]
    let digitOrHyphen = Array.append digits [| '-' |]
    let parseNumbers (x: string) =
        let startIndex, endIndex = x.IndexOfAny digitOrHyphen, x.LastIndexOfAny digits
        let temp = x.[startIndex..endIndex]
        temp.Split(',') |> Seq.map int64 |> Seq.toList
    match line.Split(' ') with
    | [| p; v; a |] -> 
        parseNumbers p, parseNumbers v, parseNumbers a
    | _ -> failwith "ups"

let sample = 
    [
    "p=<3,0,0>, v=<2,0,0>, a=<-1,0,0>"
    "p=<4,0,0>, v=<0,0,0>, a=<-2,0,0>"
    ]
    |> List.map parseLine

let uncurry f (a,b) = f a b

let zipMap a b f = List.zip a b |> List.map (uncurry f) 

let step (p,v,a) =
    let v' = zipMap v a (+)
    let p' = zipMap p v' (+)
    p', v', a

let rec solve1 particles i = 
    if i > 0 then
        let particles' = particles |> List.map step
        solve1 particles' (i-1)
    else
        particles 
        |> List.map (fun (p,_,_) -> p |> List.sumBy abs)
        |> List.indexed
        |> List.minBy (snd)
        |> fst


solve1 sample 5

let input = 
    File.ReadAllLines "20-input.txt"
    |> Seq.map parseLine
    |> Seq.toList

solve1 input 10000


let rec solve2 particles i = 
    if i > 0 then
        let particles' = 
            particles 
            |> List.map step
            |> List.groupBy (fun (p,_,_) -> p)
            |> List.choose (function | _, [ particle ] -> Some particle | _ -> None)
        solve2 particles' (i-1)
    else
        particles.Length

let sample2 = 
    [ "p=<-6,0,0>, v=<3,0,0>, a=<0,0,0>"
      "p=<-4,0,0>, v=<2,0,0>, a=<0,0,0>"
      "p=<-2,0,0>, v=<1,0,0>, a=<0,0,0>"
      "p=<3,0,0>, v=<-1,0,0>, a=<0,0,0>"
    ]
    |> List.map parseLine

solve2 sample2 10

solve2 input 10000