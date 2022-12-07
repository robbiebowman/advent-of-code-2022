// Preamble
let readLines filePath = System.IO.File.ReadLines(filePath)

let line =
    Seq.toList (readLines "input.txt")
    |> List.head
    |> Seq.toList

// Part 1
let allUnique cs = Set cs |> Set.count = (Seq.length cs)

let uniqueCharsIndex windowSize (cs: char list) =
    windowSize + Seq.findIndex (fun i -> allUnique cs[i..(i + windowSize - 1)]) [0..(List.length cs)]

printfn "%i" (uniqueCharsIndex 4 line) // 1140

// Part 2
printfn "%i" (uniqueCharsIndex 14 line) // 3495