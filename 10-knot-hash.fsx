open System

let inputPuzzle = "230,1,2,221,97,252,168,169,57,99,0,254,181,255,235,167"
let input = 
    inputPuzzle.Split(',')
    |> Array.map int
    |> Array.toList

// let reverse index length (xs: 'a list) = 
//     if length <= 1 then
//         xs
//     else
//         let toIndex = ((index + length - 1) % xs.Length)
//         if index < toIndex then
//             xs.[0..index-1] @ 
//             (xs.[index..toIndex] |> List.rev) @
//             xs.[toIndex+1..]
//         else
//             let revList = xs.[index..] @ xs.[0..toIndex] |> List.rev
//             let wrapPoint = xs.Length - index
//             revList.[wrapPoint..] @
//             xs.[toIndex+1..index-1] @
//             revList.[0..wrapPoint-1]

let reverse2 index length (xs: 'a list) = 
    let xs = List.toArray xs
    let swap a b =
        let temp = xs.[a]
        xs.[a] <- xs.[b]
        xs.[b] <- temp

    let toIndex = ((index + length - 1) % xs.Length)
    let rec loop a b iterations =
        if iterations > 0 then
            swap a b
            loop ((a + 1) % xs.Length) (if b = 0 then xs.Length - 1 else b - 1) (iterations  - 1)
        else
            xs |> List.ofArray
    loop index toIndex (length / 2)

let rec proc (list: int list) currentPos skipSize lengths = 
    match lengths with
    | [] -> 
        list, currentPos, skipSize
    | length :: tail ->
        let list' = list |> reverse2 currentPos length
        proc list' ((currentPos + length + skipSize) % list.Length) (skipSize + 1) tail

proc [0..4] 0 0 [3;4;1;5] |> fun (l,_,_) -> l.[0] * l.[1]
proc [0..255] 0 0 input |> fun (l,_,_) -> l.[0] * l.[1]

let rec proc2 (list: int list) currentPos skipSize lengths rounds =
    if rounds > 0 then
        let newList, newPos, newSkipSize = proc list currentPos skipSize lengths
        proc2 newList newPos newSkipSize lengths (rounds - 1)
    else
        list, currentPos, skipSize

let sparseToDenseHash (numbers: int list) =
    numbers
    |> List.map byte
    |> List.chunkBySize 16
    |> List.map ((List.reduce (^^^)) >> int)

[65 ; 27 ; 9 ; 1 ; 4 ; 3 ; 40 ; 50 ; 91 ; 7 ; 6 ; 0 ; 2 ; 5 ; 68 ; 22 ]
|> sparseToDenseHash

let inputPuzzle2 =
    (inputPuzzle.ToCharArray()
    |> Seq.map int
    |> Seq.toList
    ) 
    @ [ 17; 31; 73; 47; 23 ]
    
let sparseHash,_,_ = proc2 [0..255] 0 0 inputPuzzle2 64
let denseHash = sparseToDenseHash sparseHash

let hex = denseHash |> List.map (fun x -> x.ToString("x2")) |> String.concat ""
