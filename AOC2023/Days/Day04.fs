module Days.Day04

open System

let parse = System.IO.File.ReadAllLines >> Seq.cast<string>

let parseCard (card: string) =
    card[card.IndexOf(':') + 1 ..].Split(" | ")
    |> Seq.map (fun (nums: string) -> nums.Split(" ", StringSplitOptions.RemoveEmptyEntries) |> Set.ofSeq)
    |> Seq.reduce Set.intersect

let partOne =
    Seq.map parseCard
    >> Seq.map (fun intersection -> pown 2 <| Set.count intersection - 1)
    >> Seq.sum

let partTwo (cards: seq<string>) =
    let wins =
        Seq.map parseCard cards
        |> Seq.mapi (fun i intersection -> i, Set.count intersection)

    let pile = Array.create (Seq.length wins) 1

    for (i, n) in wins do
        for i' in (i + 1) .. i + n do
            pile[i'] <- pile[i'] + pile[i]

    Array.sum pile

let run path =
    let parsed = parse path
    (partOne parsed, partTwo parsed)
