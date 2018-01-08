open System

let phrase = ""

let phrase2words (phrase: string) = 
  phrase.Split(' ')

let containsNoDuplicates words = Array.distinct words |> Array.length = words.Length

let phrases = IO.File.ReadAllLines("4-words.txt")

phrases
|> Array.map phrase2words
|> Array.filter containsNoDuplicates
|> Array.length

// Part 2

let containsAnagrams (words: string[]) =
  let sortedWords =
    words 
    |> Array.map (fun word -> word.ToCharArray() |> Array.sort)

  let distinctSortedWords = sortedWords |> Array.distinct

  sortedWords.Length <> distinctSortedWords.Length

let test = phrase2words >> containsAnagrams

test "iiii oiii ooii oooi oooo"
test "oiii ioii iioi iiio"

phrases
|> Array.map phrase2words
|> Array.filter (containsAnagrams >> not)
|> Array.length