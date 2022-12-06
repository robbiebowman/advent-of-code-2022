open System

// Preamble
let readLines filePath = System.IO.File.ReadLines(filePath)

let lines =
    Seq.toList (readLines "input.txt")

// Part 1
type Instruction = int * int * int // moveAmount * from * to

let stackLines, instructionText =
    lines
    |> List.splitAt (Seq.findIndex (fun l -> l = "") lines)
    |> function
        | s, i -> Seq.toList s[0 .. (Seq.length s - 2)], Seq.toList i.Tail

let stacks =
    stackLines
    |> List.map (fun l ->
        List.chunkBySize 4 (Seq.toList l)
        |> List.map (fun cs -> cs[1]))
    |> function
        | s ->
            (let xs = Seq.length (Seq.head s) - 1
             let ys = Seq.length s - 1

             [ 0..xs ]
             |> Seq.map (fun x -> Seq.map (fun y -> s[y][x]) [ 0..ys ]))
    |> Seq.map (Seq.filter (fun c -> c <> ' ')>>Seq.toList)
    |> Seq.toList

let instructions =
    instructionText
    |> List.map (fun i ->
        i.Split([| ' ' |])
        |> Seq.filter (Seq.forall Char.IsDigit)
        |> Seq.map int
        |> Seq.toList
        |> function
            | [ x; y; z ] -> Instruction(x, y - 1, z - 1) // Change to indexes
            | _ -> raise (Exception "Weird instruction"))

let rec moveCrate (instruction:Instruction) (stackRows:char list list) =
    let amount, origin, dest = instruction
    if amount = 0 then stackRows
    else (
        let movingCrate = Seq.head stackRows[origin]
        let newCrates = List.mapi (fun n s ->
                match n = origin with
                | true -> List.tail s
                | false -> match n = dest with
                            | true -> movingCrate :: s
                            | false -> s) stackRows
        moveCrate (amount - 1, origin, dest) newCrates
        )
    
let rec doInstructions task instructions stack =
    if instructions = [] then stack
    else doInstructions task (List.tail instructions) (task (List.head instructions) stack)

let getMessage stack = List.map List.head stack |> List.map string |> String.Concat

let message = getMessage (doInstructions moveCrate instructions stacks)

printfn "%s"  message // FWNSHLDNZ

// Part 2
let moveCrateV2 (instruction:Instruction) (stackRows:char list list) =
    let amount, origin, dest = instruction
    if amount = 0 then stackRows
    else (
        let movingCrates = List.take amount stackRows[origin]
        List.mapi (fun n s ->
                match n = origin with
                | true -> List.skip (List.length movingCrates) s
                | false -> match n = dest with
                            | true -> movingCrates @ s
                            | false -> s) stackRows
        )
    
let messageTwo = getMessage (doInstructions moveCrateV2 instructions stacks)

printfn "%s"  messageTwo // FWNSHLDNZ