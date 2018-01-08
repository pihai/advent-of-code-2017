open System

let sampleInstructions = "0
3
0
1
-3"

let parseInstructions (input: string) =
  input.Split('\n')
  |> Array.map Int32.Parse

let rec jump index count (instructions: int[])  = 
  let current = instructions.[index]
  instructions.[index] <- instructions.[index] + 1
  let nextIndex = index + current
  if nextIndex < instructions.Length then
    jump nextIndex (count+1) instructions
  else
    count

parseInstructions sampleInstructions
|> jump 0 1

IO.File.ReadAllLines("5-jump-list.txt")
|> Array.map Int32.Parse
|> jump 0 1

// Part 2

let rec jump2 index count (instructions: int[])  = 
  let current = instructions.[index]
  instructions.[index] <- instructions.[index] + (if current >= 3 then -1 else 1)
  let nextIndex = index + current
  if nextIndex < instructions.Length then
    jump2 nextIndex (count+1) instructions
  else
    count

parseInstructions sampleInstructions
|> jump2 0 1

IO.File.ReadAllLines("5-jump-list.txt")
|> Array.map Int32.Parse
|> jump2 0 1