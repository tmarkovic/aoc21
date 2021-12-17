open System.IO

//let arr = [ 16; 1; 2; 0; 4; 2; 7; 1; 2; 14 ]
let arr =
    @"day7.txt"
    |> File.ReadAllText
    |> (fun x -> x.Split(','))
    |> Array.toList
    |> List.map int

let res =
    [ 0 .. (List.length arr) ]
    |> List.map (fun x ->
        arr
        |> List.map (fun y -> [ 0 .. (abs (x - y)) ] |> List.sum))
    |> List.map List.sum
    |> List.min
