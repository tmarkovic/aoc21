open System.IO
open System

let binToDec ls =
    ls
    |> List.rev
    |> List.mapi (fun i v -> (double v) * (2.0 ** i))
    |> List.sum

let filterBits oxygen =
    let sortFn =
        if oxygen then
            List.sortByDescending
        else
            List.sortBy

    let compFn =
        if oxygen then
            List.maxBy
        else
            List.minBy

    (fun ls (i: int) ->
        let filterBit =
            ls
            |> List.map (List.item i)
            |> List.countBy id
            |> sortFn fst
            |> compFn snd
            |> fst


        ls
        |> List.filter (fun numberls -> (List.item i numberls) = filterBit))

let solve list fn =
    let rec filterSeq ls i =
        match ls with
        | _ :: [] -> ls
        | _ -> filterSeq (fn ls i) (i + 1)

    filterSeq list 0

let input =
    @"day3.txt"
    |> File.ReadAllLines
    |> Array.toList
    |> List.map (fun s -> List.map int (List.ofSeq s |> List.map string))

let oxygen =
    solve input (filterBits true)
    |> List.head
    |> binToDec


let co2 =
    solve input (filterBits false)
    |> List.head
    |> binToDec

let res = oxygen * co2

()
