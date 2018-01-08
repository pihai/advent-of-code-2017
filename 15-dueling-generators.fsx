open System

let generator startValue factor criteria =
    Seq.scan (fun prevValue _ ->
        (prevValue * factor) % 2147483647L
    ) startValue (Seq.initInfinite id)
    |> Seq.skip 1
    |> Seq.filter criteria
    |> Seq.map (fun x -> 
        let bin = Convert.ToString(x, 2)
        bin.Substring(max 0 (bin.Length - 16))
    )

let noCriteria _ = true

let generatorAPart1 = generator 516L 16807L noCriteria
let generatorBPart1 = generator 190L 48271L noCriteria
// let generatorAPart1 = generator 65L 16807L noCriteria
// let generatorBPart1 = generator 8921L 48271L noCriteria
let curry f (a,b) = f a b 

let solution1 =
    Seq.zip generatorAPart1 generatorBPart1
    |> Seq.take 40000000
    |> Seq.filter (curry (=))
    |> Seq.length


let dividableBy y x = (x % y) = 0L

let generatorAPart2 = generator 516L 16807L (dividableBy 4L)
let generatorBPart2 = generator 190L 48271L (dividableBy 8L)
// let generatorAPart2 = generator 65L 16807L (dividableBy 4L)
// let generatorBPart2 = generator 8921L 48271L (dividableBy 8L)

let solution2 =
    Seq.zip generatorAPart2 generatorBPart2
    |> Seq.take 5000000
    |> Seq.filter (curry (=))
    |> Seq.length

// (65L * 16807L) % 2147483647L
// (1092455L * 16807L) % 2147483647L
// (1181022009L * 16807L) % 2147483647L
// (245556042L * 16807L) % 2147483647L
// (1744312007L * 16807L) % 2147483647L

let generatorAPart2 = 