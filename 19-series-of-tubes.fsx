open System
open System.IO

type Direction = 
    | Up | Down | Right | Left

type Diagram = char[,]
let nextPosition (x,y) = function
    | Down  -> x+1, y
    | Up    -> x-1, y
    | Right -> x, y+1
    | Left  -> x, y-1

let at (x,y) (arr: 'a[,]) = Array2D.get arr x y

let tryToTurn (x,y) (diag: Diagram) dir =
    let isNoSpaceAt p = at p diag <> ' '
    match dir with
    | Down | Up ->
      let rightPos = nextPosition (x,y) Right
      if isNoSpaceAt rightPos then
        Some Right
      else
        let leftPos = nextPosition (x,y) Left
        if isNoSpaceAt leftPos then
          Some Left
        else
          None        
    | Left | Right ->
      let upPos = nextPosition (x,y) Up
      if isNoSpaceAt upPos then
        Some Up
      else
        let downPos = nextPosition (x,y) Down
        if isNoSpaceAt downPos then
          Some Down
        else
          None      

let intialSteps = 
  [ Up; Down; Left; Right]
  |> List.map (fun d -> d, 0)
  |> Map.ofList

let incrementSteps d steps =
  let v = steps |> Map.find d
  steps
  |> Map.add d (v+1)

let sumSteps = Map.toSeq >> (Seq.sumBy snd)

let rec walk dir (x,y) letters (diag: Diagram) steps = 
  let steps = incrementSteps dir steps
  let x', y' = nextPosition (x,y) dir
  let nextChar = diag.[x',y']
  let canContinue = nextChar <> ' '

  if canContinue then
    let letters = if nextChar |> Char.IsLetter then (nextChar :: letters) else letters
    walk dir (x',y') letters diag steps
  else
    match tryToTurn (x,y) diag dir with
    | Some newDir -> 
      let x', y' = nextPosition (x,y) newDir
      let nextChar = diag.[x',y']
      let letters = if nextChar |> Char.IsLetter then (nextChar :: letters) else letters
      walk newDir (x',y') letters diag steps
    | None -> letters |> List.rev, steps |> sumSteps

let sample = 
  [
  "     |          "
  "     |  +--+    "
  "     A  |  C    "
  " F---|----E|--+ "
  "     |  |  |  D "
  "     +B-+  +--+ "
  "                "
  ]
  |> Seq.map (fun x -> x.ToCharArray())
  |> array2D

let findStartCol (diag: Diagram) = diag.[0,0..] |> Array.findIndex ((=) '|')

walk Down (0, findStartCol sample) [] sample intialSteps

let input = 
  File.ReadAllLines("19-input.txt")
  |> Seq.map (fun x -> x.ToCharArray())
  |> array2D

let solution1 = 
  walk Down (0, findStartCol input) [] input intialSteps
  |> fst |> List.toArray |> String

let solution2 = 
  walk Down (0, findStartCol input) [] input intialSteps
  |> snd