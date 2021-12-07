open System.IO
open System

let input =
    @"day4.txt" |> File.ReadAllLines |> Array.toList

let numbers =
    input
    |> List.head
    |> (fun x -> x.Split(','))
    |> Array.toList
    |> List.map int

let checkWin ls = 
                ls 
                |> List.exists (fst >> (=) false)
                |> not

let checkBoard (b: 'a list list) =
    let colWin = b |> List.transpose |> List.map checkWin |> List.exists (fun x -> x = true)
    let rowWin = b |> List.map checkWin |> List.exists (fun x -> x = true)
    rowWin || colWin

let mapm fn m = m |> List.map (fun x -> List.map fn x ) 

let marknumber n board =
                    board
                    |> mapm (fun (marked, x) -> if x = n then (true,x) else (marked,x))

let boards =
    input
    |> List.tail
    |> List.filter (String.IsNullOrEmpty >> not)
    |> List.map (fun x ->
        x.Split(" ")
        |> Array.filter (String.IsNullOrEmpty >> not)
        |> Array.map (fun y -> (false,int y))
        |> Array.toList
        )
    |> List.chunkBySize 5

let rec correctBoards (ns: int list) (bs: (bool * int) list list list) =
        match ns with
        | [] -> (None,None)
        | drawNumber::tail ->
                let marked = bs |> List.map (marknumber drawNumber)

                match ( marked |> List.tryFind (checkBoard)) with
                | None -> correctBoards tail marked
                | Some _ as res -> (res, Some drawNumber)

let correctedBoard = boards |> correctBoards numbers

let unmarked = 
        match fst correctedBoard with
        | Some x -> List.concat x |> List.filter (fst >> (=) false) |> List.sumBy snd
        | None -> 0
let last = match snd correctedBoard with
            | Some x -> x
            | None -> 0


let result = (unmarked * last)

()
