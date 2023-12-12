module Days.Day11

open System

let split (d: string) (s: string) =
    s.Split(d, StringSplitOptions.RemoveEmptyEntries)

let parse =
    System.IO.File.ReadAllLines
    >> Seq.cast<string>

let rec (>=<=) x = function
    | (lo, hi) when hi < lo -> x >=<= (hi, lo)
    | (lo, hi) -> x >= lo && x <= hi

let withI<'a> : 'a seq -> (int * 'a) seq  = Seq.mapi (fun i j -> i, j)
let is<'a> : 'a seq -> int seq = Seq.mapi (fun i _ -> i)
let indexSet = is >> Set.ofSeq

let emptiesAndGalaxies universe : int Set * int Set * (int * int) Set =
    withI universe
    |> Seq.fold
        (fun (emptyRows, emptyCols, galaxies) (x, row) ->
            let galIs =
                withI row
                |> Seq.choose (fun (y, c) -> match c with '#' -> Some y | _ -> None)
                |> Set.ofSeq

            let newEmptyRows = if Set.isEmpty galIs then Set.add x emptyRows else emptyRows

            newEmptyRows, Set.difference emptyCols galIs, Set.map (fun y -> x, y) galIs |> Set.union galaxies)
        (Set.empty, Seq.head universe |> indexSet, Set.empty)

let calculateDistances expansion universe =
    let emptyRows, emptyCols, galaxies = emptiesAndGalaxies universe

    Set.fold
        (fun (acc, gals) (x, y) ->
             let otherGalaxies = Set.remove (x, y) gals
             let mutable localSum = 0L

             for (x', y') in otherGalaxies do
                 let xExpand = Set.filter (fun x'' -> x'' >=<= (x, x')) emptyRows |> Set.count |> int64
                 let yExpand = Set.filter (fun y'' -> y'' >=<= (y, y')) emptyCols |> Set.count |> int64
                 let xDist = (abs (x - x') |> int64) - xExpand + xExpand * expansion
                 let yDist = (abs (y - y') |> int64) - yExpand + yExpand * expansion
                 localSum <- localSum + xDist + yDist

             acc + localSum, otherGalaxies)
        (0L, galaxies)
        galaxies
    |> fst


let partOne universe =
    calculateDistances 1L universe

let partTwo universe =
    calculateDistances 1_000_000L universe

let run path =
    let parsed = parse path
    partOne parsed, partTwo parsed
