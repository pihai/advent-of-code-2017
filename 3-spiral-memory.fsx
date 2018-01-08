open System

let produceCoordinates radius = 
  if radius > 0 then
    seq {
      for y in radius - 1 .. -1 .. -radius do
        yield radius, y
        
      for x in radius - 1 .. -1 .. -radius do
        yield x, -radius

      for y in -radius  + 1 .. radius do
        yield -radius, y

      for x in -radius + 1 .. radius do
        yield x, radius
    }
  else
    [ 0,0 ] :> seq<_>  

produceCoordinates 0 |> Seq.toList
produceCoordinates 1 |> Seq.toList
produceCoordinates 2 |> Seq.toList
produceCoordinates 3 |> Seq.toList

let cooridnates =
  Seq.initInfinite id
  |> Seq.collect produceCoordinates

let findCooridnateForCell cellNr =
  cooridnates |> Seq.item (cellNr - 1)

findCooridnateForCell 7

let calculateStepLength nr =
  let x,y = findCooridnateForCell nr
  abs x + abs y

calculateStepLength 1
calculateStepLength 12
calculateStepLength 23
calculateStepLength 1024
calculateStepLength 289326

// Part 2

let getAdjacentLocations (x,y) = [ for x' in -1 .. 1 do for y' in -1 .. 1 do yield x + x', y + y']

let rec loop state n =
  seq {
    printfn "-------------"
    let c = findCooridnateForCell n
    printfn "c = %A" c
    let neighbors = c |> getAdjacentLocations
    printfn "n = %A" neighbors
    printfn "state = %A" state
    let sum =
      c
      |> getAdjacentLocations
      |> List.sumBy (fun neighbor -> state |> Map.tryFind neighbor |> Option.defaultValue 0)
      |> max 1
    printfn "sum = %A" sum
    yield n, sum
    yield! loop (state.Add(c, sum)) (n+1)
    ()
  }

findCooridnateForCell 1

loop Map.empty 1 |> Seq.skipWhile (fun (x, sum) -> sum < 289326) |> Seq.head