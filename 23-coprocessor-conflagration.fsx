open System
open System.IO
open System.Windows.Forms.VisualStyles

type RegisterIdentifier = char

type Value =
    | Register of RegisterIdentifier
    | Number of int64

type BinaryOperation = RegisterIdentifier * Value

type Instruction = 
    | Set of BinaryOperation
    | Sub of BinaryOperation
    | Mul of BinaryOperation
    | Mod of BinaryOperation
    | Jump of Value * Value

type Registers = Map<char, int64>

let parseValue v = 
    match Int64.TryParse v with
    | true, value -> Number (value)
    | false, _ -> Register (v.[0])

let parseInstruction (text: string) =
    let parts = text.Split(' ')
    let reg = parts.[1].[0]
    let value2 () = parseValue parts.[2]
    match parts.[0] with
    | "set" -> Set (reg, value2())
    | "sub" -> Sub (reg, value2())
    | "mul" -> Mul (reg, value2())
    | "mod" -> Mod (reg, value2())
    | "jnz" -> Jump (parseValue parts.[1], value2())
    | x -> failwithf "Instruction %A is unknown" x

let getRegValue (reg: RegisterIdentifier) = 
    Map.tryFind reg >> (Option.defaultValue 0L)

let getValue (v: Value) (regs: Registers) =
    match v with
    | Register r -> getRegValue r regs 
    | Number n -> n

let execute (instructions: Instruction[]) initialRegs =
    let rec loop index (regs: Registers) mulCount =
        if index < instructions.Length && index >= 0 then
            match instructions.[index] with
            | Set (reg, v) -> 
                loop (index+1) (regs.Add(reg, getValue v regs)) mulCount
            | Sub (reg, v) -> 
                let result = (getRegValue reg regs) - (getValue v regs)
                loop (index+1) (regs.Add(reg, result)) mulCount
            | Mul (reg, v) -> 
                let result = (getRegValue reg regs) * (getValue v regs)
                loop (index+1) (regs.Add(reg, result)) (mulCount + 1)
            | Mod (reg, v) -> 
                let result = (getRegValue reg regs) % (getValue v regs)
                loop (index+1) (regs.Add(reg, result)) mulCount
            | Jump (v1, v2) when getValue v1 regs <> 0L -> 
                loop (index + int (getValue v2 regs)) regs mulCount
            | Jump _ -> 
                loop (index + 1) regs mulCount
        else
            mulCount, regs        
    loop 0 initialRegs 0

let instructions = 
    IO.File.ReadAllLines "23-input.txt"
    |> Array.map parseInstruction

let solution1 = execute instructions Map.empty |> fst

let solution2 = 
    let mutable f = 0
    let mutable h = 0
    let start = 790000000
    for b in start .. 17 .. start + 17000 do
        for d in 2 .. b do
            for e in 2 .. b do
                if d * e = b then
                    f <- 0
        if f = 0 then
            h <- h + 1

let isPrime n =
    match n with
    | _ when n > 3 && (n % 2 = 0 || n % 3 = 0) -> false
    | _ ->
        let maxDiv = int(System.Math.Sqrt(float n)) + 1
        let rec f d i = 
            if d > maxDiv then 
                true
            else
                if n % d = 0 then 
                    false
                else
                    f (d + i) (6 - i)     
        f 5 2

let solution2_1 = 
    let mutable h = 0
    let start = 107900

    [ start .. 17 .. start + 17000 ]
    |> List.filter (isPrime >> not)
    |> List.length

let is = IO.File.ReadAllLines "23-input.txt"

let connection i1 i2= 
    sprintf "\"(%d) %s\" -> \"(%d) %s\"" i1 is.[i1] (i2) is.[i2]

let connection1 i = connection i (i+1)

let graphviz = 
    [ 0 .. is.Length - 1 ]
    |> Seq.collect (fun i ->
        if is.[i].StartsWith("jnz") then
            let offset = is.[i].Split(' ').[2] |> int
            let dest = i + offset
            if dest < is.Length then
                if i < is.Length - 2 then
                    [ connection1 i; connection i dest ]
                else
                    [ connection i dest ]
            else
                [ sprintf "\"(%d) %s\" [shape=\"doublecircle\"]" i is.[i] ]
        else
            [ connection1 i ]
    )    
    |> String.concat (Environment.NewLine)


printfn "%s" graphviz