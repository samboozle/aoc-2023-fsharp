module Days.Day03

open System

let parse =
    System.IO.File.ReadAllLines
    >> Seq.cast<string>
    >> Seq.map Seq.toArray
    >> Seq.toArray

let updateMapList key map v =
    Map.change
        key
        (function
        | Some vs -> Some(v :: vs)
        | None -> Some [ v ])
        map

/// Given a matrix of chars (the puzzle input), do the following:
/// - Find all "numbers", each being subarrays of digits marked by an x coordinate and two (starting and ending) y coordinates.
/// - Find all "symbols" (non-digit/non-"." characters)
/// - Place all numbers in a map keying row indices against lists of all numbers (y-start * y-end tuples) in a given row
/// - Place all symbols as (x, y) coordinates in a Set
/// - Return the map and set as a tuple
let numAndSymLocations (lines: array<array<char>>) =
    let (_, numLocations, symLocations) =
        Array.fold
            (fun (x, nums: Map<int, list<int * int>>, syms) line ->
                let _, num, nums', syms' =
                    Array.fold
                        (fun (y, num, nums', syms') ch ->
                            match (ch, num) with
                            | '.', None -> (y + 1, num, nums', syms')
                            | '.', Some pos -> (y + 1, None, updateMapList x nums' pos, syms')
                            | d, None when Char.IsDigit d -> (y + 1, Some(y, y), nums', syms')
                            | d, Some(start, _) when Char.IsDigit d -> (y + 1, Some(start, y), nums', syms')
                            | _, None -> (y + 1, num, nums', Set.add (x, y) syms')
                            | _, Some pos -> (y + 1, None, updateMapList x nums' pos, Set.add (x, y) syms'))
                        (0, None, nums, syms)
                        line

                (x + 1, Option.defaultValue nums' <| Option.map (updateMapList x nums') num, syms'))
            (0, Map.empty, Set.empty)
            lines

    (numLocations, symLocations)

/// Given the x, y-start, and y-end coordinates for any number in an engine diagram,
/// return the number as an integer
let findNumber x (y, y') (engine: array<array<char>>) =
    engine[x][y..y'] |> String.Concat |> int

/// Given the x coordinate and two y coordinates, find all neighbors in a matrix for a cell of any width >= 1
let neighbors x (y, y') =
    seq {
        yield (x, y - 1)
        yield (x, y' + 1)

        yield!
            seq {
                for i in (y - 1) .. (y' + 1) do
                    yield (x - 1, i)
                    yield (x + 1, i)
            }
    }

/// Given an engine diagram as a matrix of characters, sum all numbers neighboring at least one symbol
let partOne engine =
    let (nums, syms) = numAndSymLocations engine

    Map.fold
        (fun sum x nums' ->
            List.fold
                (fun sum' pos ->
                    if Seq.exists (fun pos' -> Set.contains pos' syms) (neighbors x pos) then
                        (findNumber x pos engine) + sum'
                    else
                        sum')
                sum
                nums')
        0
        nums

/// Given an engine diagram as a matrix of characters:
/// - find all "gears" ("*" characters neighboring exactly two numbers)
/// - for each gear, find its "gear ratio" (the product of its neighboring numbers)
/// - sum all gear ratios
let partTwo engine =
    let (nums, syms) = numAndSymLocations engine

    Seq.choose
        (fun (x, y) ->
            match engine[x][y] with
            | '*' ->
                let filtered =
                    Map.fold
                        (fun ps x' nums' ->
                            List.choose
                                (fun pos ->
                                    if neighbors x' pos |> Seq.exists ((=) (x, y)) then
                                        Some <| findNumber x' pos engine
                                    else
                                        None)
                                nums'
                            @ ps)
                        []
                        nums

                match List.length filtered with
                | 2 -> Some <| List.reduce (*) filtered
                | _ -> None


            | _ -> None)
        syms
    |> Seq.sum

let run path =
    let parsed = parse path
    (partOne parsed, partTwo parsed)
