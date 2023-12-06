module Days.Day01

open System

let parse = System.IO.File.ReadAllLines >> Seq.cast<string>

// part one
let partOne =
    Seq.map (fun line ->
        let digits = Seq.filter Char.IsDigit line
        sprintf "%c%c" (Seq.head digits) (Seq.last digits) |> int)
    >> Seq.sum

// part two
let digitValues =
    [ 1..9 ] @ [ 1..9 ]
    |> List.zip (
        [ "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9" ]
        @ [ "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine" ]
    )

let rec intoFirstAndLastDigits (digits: list<int>) : string -> int * int =
    function
    | "" ->
        match digits with
        | [] -> failwith "No digits found."
        | xs -> (List.last xs, List.head xs)

    | line ->
        let digits' =
            List.tryFind (fun (dig: string, _) -> line.StartsWith(dig)) digitValues
            |> Option.map (fun (_, num: int) -> num :: digits)
            |> Option.defaultValue digits

        intoFirstAndLastDigits digits' line.[1..]

let partTwo =
    Seq.map (intoFirstAndLastDigits [])
    >> Seq.map (fun (x, y) -> x * 10 + y)
    >> Seq.sum

// module runner
let run data =
    let parsed = parse data
    partOne parsed, partTwo parsed
