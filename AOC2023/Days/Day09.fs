module Days.Day09

open System

let split (d: string) (s: string) =
    s.Split(d, StringSplitOptions.RemoveEmptyEntries)

let parse =
    System.IO.File.ReadAllLines
    >> Seq.cast<string>
    >> Seq.map (fun (hand: string) ->
        split " " hand
        |> Seq.map int
        |> Seq.toList
    )

let rec findPattern acc =
    function
    | xs when List.forall ((=) 0) xs -> acc
    | xs -> findPattern (xs :: acc) (List.pairwise xs |> List.map (fun (a, b) -> b - a))

let solveWith endFn reduction =
    Seq.map ((findPattern []) >> (List.map endFn) >> reduction) >> Seq.sum

let partOne = solveWith List.last List.sum
let partTwo = solveWith List.head <| List.reduce (fun acc el -> el - acc)

let run path =
    let parsed = parse path
    partOne parsed, partTwo parsed
