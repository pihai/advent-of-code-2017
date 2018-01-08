open System

let sampleBanks = [| 0; 2; 7; 0 |]

let getElementAt i (xs: 'a[]) = xs.[i % xs.Length]
let nextIndex i xs = ((i + 1) % Array.length xs)

nextIndex 2 sampleBanks
nextIndex 3 sampleBanks

let chooseBank = 
  Array.indexed >> 
  Array.sortByDescending (fun (index, bank) -> bank, -index) >> 
  Array.head >>
  fst >>
  abs

chooseBank sampleBanks
chooseBank [| 0; 2; 7; 0; 7 |]

let redistribute (banks: int[]) index =
  let rec loop banks i blocks = 
    if blocks = 0 then
      banks
    else
      // printfn "blocks %d: %A" blocks banks
      let index = banks |> nextIndex i
      banks.[index] <- banks.[index] + 1
      loop banks index (blocks - 1)

  let blocks = banks.[index]
  banks.[index] <- 0
  loop banks index blocks

redistribute (Array.copy sampleBanks) 2

let cycle banks =
  let rec loop banks results cycle =
    let banks = Array.copy banks
    let index = chooseBank banks
    let redistributedBanks = redistribute banks index
    if results |> Set.contains redistributedBanks then
      cycle, redistributedBanks
    else
      loop redistributedBanks (results.Add redistributedBanks) (cycle + 1)

  loop banks  Set.empty 1

cycle sampleBanks

let input = [|4;10;4;1;8;4;9;14;5;1;14;15;0;15;3;5|]

let nrOfCycles, config = cycle input

// PART 2

let cycle2 startBanks = 
  let rec loop banks cycle =
    let banks = Array.copy banks
    let index = chooseBank banks
    let redistributedBanks = redistribute banks index
    if startBanks = redistributedBanks then
      cycle, redistributedBanks
    else
      loop redistributedBanks (cycle + 1)
  loop startBanks 1


let x,y = cycle2 ([| 2; 4; 1; 2 |])

let nrOfCycles2, config2 = cycle2 [|1; 0; 14; 14; 12; 11; 10; 9; 9; 7; 5; 5; 4; 3; 7; 1|]

