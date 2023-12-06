module Days.Day02

open System

let (!<<) = (<<) not

type hand = { blue: int; red: int; green: int }
type game = { gameId: int; hands: list<hand> }

let defaultHand = { blue = 0; red = 0; green = 0 }

let int' =
    Seq.skipWhile !<<Char.IsDigit
    >> Seq.takeWhile Char.IsDigit
    >> String.Concat
    >> int

let (|Blue|Green|Red|Other|) (handStr: string) =
    match handStr with
    | str when str.Contains("blue") -> Blue(int' str)
    | str when str.Contains("green") -> Green(int' str)
    | str when str.Contains("red") -> Red(int' str)
    | _ -> Other

let parseHand (handStr: string) =
    handStr.Split(",")
    |> Seq.fold
        (fun (hand: hand) (colorStr: string) ->
            match colorStr with
            | Blue blue -> { hand with blue = blue }
            | Green green -> { hand with green = green }
            | Red red -> { hand with red = red }
            | Other -> hand)
        defaultHand

let parseGame (gameStr: string) =
    let hands =
        gameStr.[(gameStr.IndexOf(":") + 1) ..].Split(";")
        |> Seq.map parseHand
        |> Seq.toList

    { gameId = int' gameStr; hands = hands }

let parse = System.IO.File.ReadAllLines >> Seq.cast<string> >> Seq.map parseGame

// part one
let isValidHand (hand: hand) =
    hand.blue <= 14 && hand.green <= 13 && hand.red <= 12

let partOne =
    Seq.choose (fun (game: game) ->
        if List.forall isValidHand game.hands then
            Some game.gameId
        else
            None)
    >> Seq.sum

// part two
let powerOfGame (game: game) =
    let { blue = blue
          green = green
          red = red }: hand =
        List.fold
            (fun acc hand ->
                { acc with
                    blue = max acc.blue hand.blue
                    red = max acc.red hand.red
                    green = max acc.green hand.green })
            defaultHand
            game.hands

    blue * green * red

let partTwo games = Seq.map powerOfGame games |> Seq.sum

let run data =
    let parsed = parse data
    (partOne parsed, partTwo parsed)
