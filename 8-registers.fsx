open System

let example = "b inc 5 if a > 1
a inc 1 if b < 5
c dec -10 if a >= 1
c inc -20 if c == 10".Split('\n')

let input = (IO.File.ReadAllLines "8-input.txt")
 
let mapCondOperator = function
  | "<" -> (<)
  | ">" -> (>)
  | "<=" -> (<=)
  | ">=" -> (>=)
  | "==" -> (=)
  | "!=" -> (<>)
  | _ -> failwith "Unknown operator"
  
let toIncFunc = function
  | "inc" -> (+) | "dec" -> (-)
  | _ -> failwith "Unknown operator"

let parseLine (line: string) =
  let parts = line.Split ' '
  let reg = parts.[0]
  let incr = parts.[1]
  let value = parts.[2] |> Int32.Parse
  let condReg = parts.[4]
  let condOperator = parts.[5]
  let condValue = parts.[6] |> Int32.Parse

  // reg, incr, value, condReg, condOperator, condValue
  reg, (fun x -> (toIncFunc incr) x value), condReg, (mapCondOperator condOperator), condValue
 
((-) -10) 0

example |> Array.map parseLine
 


input
|> Array.map parseLine
|> Array.fold (fun (maxValue, registers) (reg, incrFunc, condReg, condOp, condValue) ->
  let getValue x =
    registers
    |> Map.tryFind x
    |> Option.defaultValue 0
  
  let updateValue key value =
    registers |> Map.remove key |> Map.add key value

  let conditionResult = condOp (getValue condReg) condValue

  if conditionResult then
    let newValue = incrFunc (getValue reg)
    max maxValue newValue, newValue |>  updateValue reg
  else
    maxValue, registers
) (0, Map.empty)
|> fun (maxValue, registers) -> 
  maxValue, 
  registers |> Map.toSeq |> Seq.maxBy snd

// dec -10 -> 10