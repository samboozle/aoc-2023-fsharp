module Days.Day05

open System

let split (d: string) (s: string) =
    s.Split(d, StringSplitOptions.RemoveEmptyEntries) |> Seq.toList

let (|ToCoords|) (lines: list<string>) =
    ToCoords(
        List.map
            (fun (line: string) ->
                match split " " line |> List.map int64 with
                | a :: b :: c :: [] -> (a, b, c)
                | _ -> failwith "invalid coordinate format")
            lines
        |> List.sortBy (fun (_, x, _) -> x)
    )

let parse path : list<int64> * Map<string, list<(int64 * int64 * int64)>> =
    let data = IO.File.ReadAllText path

    match split "\n\n" data with
    | seeds :: maps ->
        let seeds' = split " " seeds[seeds.IndexOf(":") + 2 ..] |> List.map int64

        let maps' =
            List.fold
                (fun acc (m: string) ->
                    match split "\n" m with
                    | key :: (ToCoords coords) -> Map.add key coords acc
                    | _ -> failwith "invalid map format")
                Map.empty
                maps

        (seeds', maps')

    | _ -> failwith "invalid data format"

let keyChain =
    [ "seed-to-soil map:"
      "soil-to-fertilizer map:"
      "fertilizer-to-water map:"
      "water-to-light map:"
      "light-to-temperature map:"
      "temperature-to-humidity map:"
      "humidity-to-location map:" ]

let followMaps keys maps seed =
    List.fold
        (fun v k ->
            Map.find k maps
            |> List.tryFind (fun (_, src, range) -> v >= src && v < src + range)
            |> Option.map (fun (dest, src, _) -> dest + v - src)
            |> Option.defaultValue v)
        seed
        keys

let rec iterateRanges acc (lo, hi) =
    function
    | [] -> (lo, hi) :: acc
    | (dest, src, range) :: coords ->
        let diff = dest - src
        let hi' = src + range - 1L

        let insideL = lo >= src && lo <= hi' // input range start is within src range
        let insideR = hi <= hi' && hi >= src // input range end is within src range

        match insideL, insideR with
        // let [ ] represent the input range and ( ) represent the source range
        // input range is below source range -> [ ] ( )
        | false, false when src > hi -> (lo, hi) :: acc
        // input range is above source range -> ( ) [ ]
        | false, false when hi' < lo -> iterateRanges acc (lo, hi) coords
        // input range encompases source range -> [ ( ) ]
        | false, false -> iterateRanges ((lo, src - 1L) :: (src + diff, hi' + diff) :: acc) (hi' + 1L, hi) coords
        // input range overlaps left side of source range -> [ ( ] )
        | false, true -> (lo, hi' - 1L) :: (src + diff, hi' + diff) :: acc
        // input range overlaps right side of source range -> ( [ ) ]
        | true, false -> iterateRanges ((lo + diff, hi' + diff) :: acc) (hi' + 1L, hi) coords
        // input range is encompassed by source range ( [ ] )
        | true, true -> (lo + diff, hi + diff) :: acc

let rec followMaps' (keys: string list) (maps: Map<string, (int64 * int64 * int64) list>) (seed: int64 * int64) =
    match keys with
    | [] -> [ fst seed ]
    | k :: ks -> Map.find k maps |> iterateRanges [] seed |> List.collect (followMaps' ks maps)

let partOne ((seeds, maps): list<int64> * Map<string, list<(int64 * int64 * int64)>>) : int64 =
    List.map (followMaps keyChain maps) seeds |> List.min

let partTwo ((seeds, maps): list<int64> * Map<string, list<(int64 * int64 * int64)>>) : int64 =
    List.chunkBySize 2 seeds
    |> List.choose (function
        | [ seed; len ] -> Some(seed, seed + len - 1L)
        | _ -> None)
    |> List.collect (followMaps' keyChain maps)
    |> List.min

let run path =
    let parsed = parse path

    (partOne parsed, partTwo parsed)
