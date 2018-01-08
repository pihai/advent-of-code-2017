open System
open System.Text.RegularExpressions

let regex = Regex("(?<prog>[a-z]+) \((?<nr>\d+)\)( -> (?<child>[a-z]+)((, )(?<child>[a-z]+))*)?")

let parseLine line =
  let result = regex.Match line
  let prog = result.Groups.["prog"].Value
  let nr = result.Groups.["nr"].Value
  let children = 
    result.Groups.["child"].Captures 
    |> Seq.cast<Capture>
    |> Seq.map (fun c -> c.Value)
    |> Seq.toList
  prog, Int32.Parse nr, children

parseLine "dsiixv (52)"
parseLine "dsiixv (52) -> asdf, xyz"

let parseEntries file =
  IO.File.ReadAllLines file
  |> Array.map parseLine
  |> Array.toList

let sampleEntries = parseEntries "7-sampleInput.txt"
let input = parseEntries "7-input.txt"

let findRoot (entries: (string * string list) list) =
  let allChildren =
    entries
    |> List.collect snd
    |> List.distinct
    |> Set.ofList

  let allEntries =
    entries
    |> List.map (fun (x,_) -> x)
    |> Set.ofList

  allEntries - allChildren
  

let root = 
  input
  |> List.map (fun (x,_,xs) -> x, xs)
  |> findRoot



let findError (input: (string * int * string list) list) =
  let parents =
    input
    |> List.filter (fun (_,_,xs) -> xs |> List.isEmpty |> not)

  let map = 
    input
    |> List.map (fun (x, y, z) -> x, (y,z))
    |> Map.ofList

  let rec getChildSize prog =
    printfn "prog = %A" prog
    let size, children = map |> Map.find prog
    let childSizes = (children |> List.sumBy getChildSize)
    size + childSizes

  let sizes =
    map
    |> Map.map (fun prog _ -> getChildSize prog)

  let weightsMap =
    sampleEntries
    |> List.map (fun (x,w,_) -> x,w)
    |> Map.ofList

  parents
  |> List.choose (fun (x,w,xs) ->
    let weights = 
      xs 
      |> List.map (fun c -> c, sizes |> Map.find c)

    let weightGroups = 
      weights
      |> List.groupBy (fun (_, size) -> size)
      
    if weightGroups.Length <> 1 then // i'm too heavy
      
      let expectedLength = 
        weightGroups
        |> List.maxBy (fun (size, progs) -> progs.Length)
      
      let wrongLength =
        weightGroups
        |> List.minBy (fun (size, progs) -> progs.Length)

      let diff = (fst expectedLength) - (fst wrongLength)

      let wrongProg = wrongLength |> snd |> List.head |> fst

      printfn "%A" (weightsMap.[wrongProg] - (abs diff))
      Some (weightsMap.[wrongProg] - (abs diff))
    else
      None
  )

findError sampleEntries
findError input

  // sampleEntries
  // |> List.filter (fun (_,_,xs) -> xs |> List.isEmpty |> not)
  // |> List.map (fun (x,w,xs) ->
  //   x, w, getChildrenWeight weights xs
  // )




let foo = (<=) 5

[ 1 .. 10 ]
|> List.forall ((<=)5)