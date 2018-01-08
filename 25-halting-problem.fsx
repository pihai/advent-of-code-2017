type TapeValue = Zero | One
type Tape = Map<int,TapeValue>
type Position = int

type State = State of (TapeValue -> (TapeValue * (int -> int) * State))

let right = (+) 1
let left = (+) (-1)

let rec aSample = State (function
    | Zero -> One, right, bSample
    | One  -> Zero, left, bSample)
and bSample = State(function
    | Zero -> One, left, aSample
    | One  -> One, right, aSample)

let curVal idx = Map.tryFind idx >> Option.defaultValue Zero

let step ((tape: Tape), pos, State state) = 
    let value, nextPos, nextState = state (tape |> curVal pos)
    tape.Add(pos, value), (nextPos pos), nextState

let count tape =
    tape
    |> Map.toList
    |> List.map snd
    |> List.countBy id

let sampleSolution1 = 
    [ 1 .. 6 ]
    |> List.fold (fun s _ -> step s) (Map.empty, 0, aSample)
    |> fun (tape,_,_) -> count tape
let rec a = State (function
    | Zero -> One, right, b
    | One  -> One, left, e)
and b = State(function
    | Zero -> One, right, c
    | One  -> One, right, f)
and c = State(function
    | Zero -> One, left, d
    | One  -> Zero, right, b)
and d = State(function
    | Zero -> One, right, e
    | One  -> Zero, left, c)
and e = State(function
    | Zero -> One, left, a
    | One  -> Zero, right, d)
and f = State(function
    | Zero -> One, right, a
    | One  -> One, right, c)

let solution1 = 
    [ 1 .. 12459852 ]
    |> List.fold (fun s _ -> step s) (Map.empty, 0, a)
    |> fun (tape,_,_) -> count tape

