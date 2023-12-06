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

type parsedMaps = (int64 * int64 * int64) list list
type parsedPath = int64 list * parsedMaps

let parse path : parsedPath =
    let data = IO.File.ReadAllText path

    match split "\n\n" data with
    | seeds :: maps ->
        let seeds' = split " " seeds[seeds.IndexOf(":") + 2 ..] |> List.map int64

        let maps' =
            List.map
                (fun (m: string) ->
                    match split "\n" m with
                    | _ :: (ToCoords coords) -> coords
                    | _ -> failwith "invalid map format")
                maps

        (seeds', maps')

    | _ -> failwith "invalid data format"

let followMaps maps seed =
    List.fold
        (fun v map ->
            List.tryFind (fun (_, src, range) -> v >= src && v < src + range) map
            |> Option.map (fun (dest, src, _) -> dest + v - src)
            |> Option.defaultValue v)
        seed
        maps

let inline (>==<) x (lo, hi) = x >= lo && x <= hi

let (|Left|Right|Contains|Contained|Below|Above|) ((lo, hi), (lo', hi'), diff) =
    match lo >==< (lo', hi'), hi >==< (lo', hi') with
    | false, false when lo' > hi -> Below
    | false, false when hi' < lo -> Above
    | false, false -> Contains((lo, lo' - 1L), (lo' + diff, hi' + diff), (hi' + 1L, hi))
    | true, true -> Contained(lo + diff, hi + diff)
    | false, true -> Left((lo, hi' - 1L), (lo + diff, hi' + diff))
    | true, false -> Right((lo + diff, hi' + diff), (hi' + 1L, hi))

let rec iterateRanges acc seed =
    function
    | [] -> seed :: acc
    | (dest, src, range) :: coords ->
        match seed, (src, src + range - 1L), dest - src with
        | Below -> seed :: acc
        | Above -> iterateRanges acc seed coords
        | Contains(a, b, seed') -> iterateRanges (a :: b :: acc) seed' coords
        | Left(a, b) -> a :: b :: acc
        | Right(a, seed') -> iterateRanges (a :: acc) seed' coords
        | Contained seed' -> seed' :: acc

let rec followMaps' (maps: parsedMaps) (seed: int64 * int64) =
    match maps with
    | [] -> [ fst seed ]
    | m :: ms -> iterateRanges [] seed m |> List.collect (followMaps' ms)

let partOne ((seeds, maps): parsedPath) : int64 =
    List.map (followMaps maps) seeds |> List.min

let partTwo ((seeds, maps): parsedPath) : int64 =
    List.chunkBySize 2 seeds
    |> List.choose (function
        | [ seed; len ] -> Some(seed, seed + len - 1L)
        | _ -> None)
    |> List.collect (followMaps' maps)
    |> List.min

let run path =
    let parsed = parse path

    (partOne parsed, partTwo parsed)
