open System
open System.Collections

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

let knotHash (input: string) =
    let rec proc (list: int list) currentPos skipSize lengths = 
        match lengths with
        | [] -> 
            list, currentPos, skipSize
        | length :: tail ->
            let list' = list |> reverse2 currentPos length
            proc list' ((currentPos + length + skipSize) % list.Length) (skipSize + 1) tail

    let rec proc2 (list: int list) currentPos skipSize lengths rounds =
        if rounds > 0 then
            let newList, newPos, newSkipSize = proc list currentPos skipSize lengths
            proc2 newList newPos newSkipSize lengths (rounds - 1)
        else
            list

    let input =
        (input.ToCharArray()
        |> Seq.map int
        |> Seq.toList
        ) 
        @ [ 17; 31; 73; 47; 23 ]

    proc2 [0..255] 0 0 input 64
    |> List.map byte
    |> List.chunkBySize 16
    |> List.map ((List.reduce (^^^)) >> int)
    |> List.map (fun x -> x.ToString("x2")) 
    |> String.concat ""

// knotHash "1,2,4" = "63960835bcdc130f0b66d7ff4f6a5a8e"
// knotHash "1,2,3" = "3efbe78a8d82f29979031a4aa0b16a9d"
// knotHash "AoC 2017" = "33efeb34ea91902bb2f59c9920caa6cd"
// knotHash "" = "a2582a3a0e66e6e86e3812dcb672a272"

let hexCharToBits = function
    | '0' -> [ 0; 0; 0; 0 ]
    | '1' -> [ 0; 0; 0; 1 ]
    | '2' -> [ 0; 0; 1; 0 ]
    | '3' -> [ 0; 0; 1; 1 ]
    | '4' -> [ 0; 1; 0; 0 ]
    | '5' -> [ 0; 1; 0; 1 ]
    | '6' -> [ 0; 1; 1; 0 ]
    | '7' -> [ 0; 1; 1; 1 ]
    | '8' -> [ 1; 0; 0; 0 ]
    | '9' -> [ 1; 0; 0; 1 ]
    | 'a' -> [ 1; 0; 1; 0 ]
    | 'b' -> [ 1; 0; 1; 1 ]
    | 'c' -> [ 1; 1; 0; 0 ]
    | 'd' -> [ 1; 1; 0; 1 ]
    | 'e' -> [ 1; 1; 1; 0 ]
    | 'f' -> [ 1; 1; 1; 1 ]

let hex2Bits (input: string) =
    input.ToCharArray()
    |> Array.toList
    |> List.collect hexCharToBits

// hex2Bits "a0c2017"

let countUsedSquares = List.sum
let solution1 = 
    [ 0 .. 127 ]
    |> List.sumBy ((sprintf "amgozmfv-%d") >> knotHash >> hex2Bits >> countUsedSquares)

let toCoordinates row (x: int list) =
    x
    |> List.mapi (fun i y -> i,y)
    |> List.filter (snd >> ((=) 1))
    |> List.map (fun (col,_) -> row, col)

type Region = Set<int * int>

let isAdjacent (x1,y1) (x2,y2) =
  (x1 = x2 && (y1 = (y2 + 1) || y1 = (y2 - 1))) ||
  (y1 = y2 && (x1 = (x2 + 1) || x1 = (x2 - 1)))
 
let areConnected (regionA: Region) (regionB: Region) =
  regionA |> Set.exists (fun a -> regionB |> Set.exists (isAdjacent a))

let rec connectUntilConvergence (regions: Set<Region>) =
  printfn "%d" regions.Count
  let x =
    regions
    |> Set.map (fun regionA ->
      regions.Remove regionA
      |> Set.filter (areConnected regionA)
      |> Set.fold (+) regionA
    //   regions.Remove regionA
    //   |> Seq.tryFind (areConnected regionA)
    //   |> Option.map ((+) regionA)
    //   |> Option.defaultValue regionA
    )

  if x.Count = regions.Count 
  then x
  else connectUntilConvergence x

let solution2 = 
    [ 0 .. 127 ]
    |> List.collect (fun row ->
        // sprintf "flqrgnkx-%d" row
        sprintf "amgozmfv-%d" row
        |> knotHash 
        |> hex2Bits 
        |> (toCoordinates row)
    )
    |> List.map Set.singleton
    |> Set.ofList
    |> connectUntilConvergence
    |> Set.count