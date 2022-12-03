// Preamble
let readLines filePath = System.IO.File.ReadLines(filePath)

let lines =
    Seq.toList (readLines "day-2-input.txt")

// Part 1
type Game = int * int

let games =
    Seq.map
        (fun l ->
            let [ x; ' '; y ] = Seq.toList l
            // rock = 0, paper = 1, scissors = 2
            Game(((int x) - 65), ((int y) - 88)))
        lines

let outcomeValue (game: Game) =
    match game with
    | x, y when x = y -> 3
    | x, y when x - y = -1 || x - y = 2 -> 6
    | _ -> 0

let handShapeValue h = h + 1

let gameValue (x, y) = outcomeValue (x, y) + handShapeValue y

let ans = Seq.map gameValue games |> Seq.sum

printfn "%i" ans // 11666

// Part 2
let decodeMove g =
    match g with
    | x, 0 when x = 0 -> x, 2 // Lose, but elf played rock
    | x, 0 -> x, x - 1
    | x, 2 when x = 2 -> x, 0 // Win, but elf played scissors
    | x, 2 -> x, x + 1
    | x, 1 -> x, x

let ansTwo =
    Seq.map decodeMove games
    |> Seq.map gameValue
    |> Seq.sum

printfn "%i" ansTwo // 12767
