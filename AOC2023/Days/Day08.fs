module Days.Day08

open System

let split (d: string) (s: string) =
    s.Split(d, StringSplitOptions.RemoveEmptyEntries)

let parse path =
    let data = IO.File.ReadAllText path

    match split "\n\n" data with
    | [| instructions; lines |] ->
        let nodes =
            split "\n" lines
            |> Array.fold (fun acc (line: string) -> Map.add line.[0..2] (line.[7..9], line.[12..14]) acc) Map.empty

        instructions, nodes

    | _ -> failwith "invalid data format"

let rec cycle xs =
    seq {
        yield! xs
        yield! cycle xs
    }

let followGraph instructions nodes (endFn: string -> bool) start =
    cycle instructions
    |> Seq.scan
        (fun node dir ->
            let (l, r) = Map.find node nodes

            match dir with
            | 'L' -> l
            | 'R' -> r
            | _ -> failwith "invalid direction")
        start
    |> Seq.takeWhile endFn
    |> Seq.length
    |> int64

let rec gcd a =
    function
    | 0L -> a
    | b -> gcd b (a % b)

let lcm a b = (a * b) / (gcd a b)

let partOne (instructions, nodes) =
    followGraph instructions nodes ((<>) "ZZZ") "AAA"

// frankly, it's silly the solution was this easy - I abandoned this line of thinking
// after realizing that it doesn't account for the period of the instruciton cycle...
// the input data is just this forgiving
let partTwo (instructions, nodes) =
    Map.toSeq nodes
    |> Seq.choose (fun (key: string, _) ->
            match key.[2] with
            | 'A' -> Some key
            | _ -> None)
    |> Seq.map (followGraph instructions nodes (fun (node: string) -> node.[2] <> 'Z'))
    |> Seq.reduce lcm

let run path =
    let parsed = parse path
    (partOne parsed, partTwo parsed)
