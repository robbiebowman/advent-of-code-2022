// Preamble
let readLines filePath = System.IO.File.ReadLines(filePath)

let lines =
    Seq.toList (readLines "day-3-input.txt")

// Part 1
type Rucksack = char list * char list

let rucksacks =
    Seq.map Seq.toList lines
    |> Seq.map (fun s -> List.chunkBySize ((List.length s) / 2) s)
    |> Seq.map (fun [ xs; ys ] -> Rucksack(xs, ys))

let rucksackMatch (xs, ys) =
    let xBag = xs |> Seq.map (fun x -> x, true) |> dict
    ys |> Seq.find xBag.ContainsKey

let priority c =
    let i = int c
    match i > 90 with
    | true -> i - 96 // Lowercase -> convert to 1 ~ 26
    | false -> i - 38 // Uppercase -> 27 ~ 52

let ans =
    rucksacks
    |> Seq.map rucksackMatch
    |> Seq.map priority
    |> Seq.sum
    
printfn "%i" ans // 8233

// Part 2
type Bag = char list
type Group = Bag * Bag * Bag

let groups =
    lines
    |> Seq.map Seq.toList
    |> Seq.chunkBySize 3
    |> Seq.map (fun t ->
        let [ xs; ys; zs ] = (Seq.toList t)
        Group(xs, ys, zs))

let groupMatch (g: Group) =
    let a, b, c = g
    let aBag = a |> Seq.map (fun x -> x, true) |> dict
    let bBag = b |> Seq.filter aBag.ContainsKey
    c |> Seq.find (fun y -> Seq.contains y bBag)

let ansTwo =
    groups
    |> Seq.map (groupMatch >> priority)
    |> Seq.sum

printfn "%i" ansTwo // 2821