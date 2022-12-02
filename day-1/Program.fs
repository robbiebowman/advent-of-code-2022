// Preamble
let readLines filePath = System.IO.File.ReadLines(filePath)

let text =
    Seq.toList (readLines "day-1-input.txt")

// Part 1
let elfFuels: List<List<int>> =
    Seq.fold
        (fun x y ->
            match y with
            | "" -> [] :: x
            | _ -> (int y :: x.Head) :: x.Tail)
        [ [] ]
        text

let fuelTotals = Seq.map Seq.sum elfFuels
printfn $"Part 1: %i{Seq.max fuelTotals}" // 66487

// Part 2
let rankedTotals =
    Seq.sortDescending fuelTotals

let totalTopThree = Seq.take 3 rankedTotals
printfn $"Part 2: %i{Seq.sum totalTopThree}" // 197301
