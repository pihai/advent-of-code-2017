open System
open System.IO
open System.Collections.Generic

type RegisterIdentifier = char

type Value =
    | Register of RegisterIdentifier
    | Number of int64

type BinaryOperation = RegisterIdentifier * Value

type Instruction = 
    | Sound of RegisterIdentifier
    | Set of BinaryOperation
    | Add of BinaryOperation
    | Mul of BinaryOperation
    | Mod of BinaryOperation
    | Recover of RegisterIdentifier
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
    | "snd" -> Sound reg
    | "set" -> Set (reg, value2())
    | "add" -> Add (reg, value2())
    | "mul" -> Mul (reg, value2())
    | "mod" -> Mod (reg, value2())
    | "rcv" -> Recover reg
    | "jgz" -> Jump (parseValue parts.[1], value2())
    | x -> failwithf "Instruction %A is unknown" x

let getRegValue (reg: RegisterIdentifier) = 
    Map.tryFind reg >> (Option.defaultValue 0L)

let getValue (v: Value) (regs: Registers) =
    match v with
    | Register r -> getRegValue r regs 
    | Number n -> n

let execute (instructions: Instruction[]) =
    let rec loop lastSound index (regs: Registers) =
        match instructions.[index] with
        | Sound reg    ->  
            loop (Some (getRegValue reg regs)) (index+1) regs
        | Set (reg, v) -> 
            loop lastSound (index+1) (regs.Add(reg, getValue v regs))
        | Add (reg, v) -> 
            let result = (getRegValue reg regs) + (getValue v regs)
            loop lastSound (index+1) (regs.Add(reg, result))
        | Mul (reg, v) -> 
            let result = (getRegValue reg regs) * (getValue v regs)
            loop lastSound (index+1) (regs.Add(reg, result))
        | Mod (reg, v) -> 
            let result = (getRegValue reg regs) % (getValue v regs)
            loop lastSound (index+1) (regs.Add(reg, result))
        | Recover reg when getRegValue reg regs <> 0L -> 
            lastSound
        | Recover _ -> 
            loop lastSound (index+1) regs
        | Jump (reg, v) when getValue reg regs > 0L -> 
            loop lastSound (index + int (getValue v regs)) regs
        | Jump _ -> 
            loop lastSound (index + 1) regs
    loop None 0 Map.empty

let sample = 
    [|
    "set a 1"
    "add a 2"
    "mul a a"
    "mod a 5"
    "snd a"
    "set a 0"
    "rcv a"
    "jgz a -1"
    "set a 1"
    "jgz a -2"
    |]
    |> Array.map parseInstruction
 
execute sample

let instructions = 
    IO.File.ReadAllLines "18-input.txt"
    |> Array.map parseInstruction

execute instructions
let execute2 (instructions: Instruction[]) = 
    let step index (regs: Registers) (sendQueue: Queue<int64>) (receiveQueue: Queue<int64>) log =
        match instructions.[index] with
        | Sound reg    -> // Send 
            sendQueue.Enqueue(getRegValue reg regs)
            (index+1), regs
        | Set (reg, v) -> 
            (index+1), (regs.Add(reg, getValue v regs))
        | Add (reg, v) -> 
            let result = (getRegValue reg regs) + (getValue v regs)
            (index+1) ,(regs.Add(reg, result))
        | Mul (reg, v) -> 
            let result = (getRegValue reg regs) * (getValue v regs)
            (index+1), (regs.Add(reg, result))
        | Mod (reg, v) -> 
            let result = (getRegValue reg regs) % (getValue v regs)
            (index+1), (regs.Add(reg, result))
        | Recover reg  when receiveQueue.Count > 0 -> // receive
            let r = receiveQueue.Dequeue()
            (index+1), (regs.Add(reg, r))
        | Recover _ -> // receive
            index, regs
        | Jump (reg, v) -> 
            if getValue reg regs > 0L then
                (index + int (getValue v regs)), regs
            else            
                (index + 1), regs

    let queue1 = Queue<int64>()
    let queue2 = Queue<int64>()

    let rec loop regs1 regs2 index1 index2 sendCount = 
        log (string index2)
        let sendCount = match instructions.[index2] with | Sound _ -> sendCount + 1 | _ -> sendCount
        let index1', regs1' = step index1 regs1 queue1 queue2 ignore
        let index2', regs2' = step index2 regs2 queue2 queue1 ignore
        match instructions.[index1], instructions.[index2] with
        | Recover _, Recover _ when queue1.Count = 0 && queue2.Count = 0 -> "deadlock", sendCount
        | _ -> loop regs1' regs2' index1' index2' sendCount
    
    let initialRegs pId = ['p',pId] |> Map.ofList
    loop (initialRegs 0L) (initialRegs 1L) 0 0 0

let sample2 = 
    [|
    "snd 1"
    "snd 2"
    "snd p"
    "rcv a"
    "rcv b"
    "rcv c"
    "rcv d"
    |]
    |> Array.map parseInstruction

// execute2 sample2

execute2 instructions