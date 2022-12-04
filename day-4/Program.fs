open System.Text.RegularExpressions

// Preamble
let readLines filePath = System.IO.File.ReadLines(filePath)

let lines =
    Seq.toList (readLines "input.txt")

// Part 1
type Range = int * int
type Pair = Range * Range

let countMatches matcher xs = xs |> Seq.filter matcher |> Seq.length

let pairs =
    lines
    |> Seq.map (fun l ->
        l.Split([| '-'; ',' |])
        |> Seq.map int
        |> Seq.toList)
    |> Seq.map (fun ns ->
        let [ a1; b1; a2; b2 ] = ns
        Pair(Range(a1, b1), Range(a2, b2)))

let subset ((x1, y1), (x2, y2)) = x2 >= x1 && y2 <= y1

let anySubset (a, b) = subset (a, b) || subset (b, a)

let subsets = countMatches anySubset pairs

printfn "%i" subsets // 547

// Part 2
let overlap ((x1, y1), (x2, y2)) =
    (x2 >= x1 && x2 <= y1) || (y2 >= x1 && y2 <= y1)

let anyOverlap (a, b) = overlap (a, b) || overlap (b, a)

let overlaps = countMatches anyOverlap pairs

printfn "%i" overlaps // 843
