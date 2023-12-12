module Days.Day07

type Card =
    | Ace
    | King
    | Queen
    | Jack
    | Ten
    | Nine
    | Eight
    | Seven
    | Six
    | Five
    | Four
    | Three
    | Two
    | Joker

let charToCard =
    function
    | 'A' -> Ace
    | 'K' -> King
    | 'Q' -> Queen
    | 'J' -> Jack
    | 'T' -> Ten
    | '9' -> Nine
    | '8' -> Eight
    | '7' -> Seven
    | '6' -> Six
    | '5' -> Five
    | '4' -> Four
    | '3' -> Three
    | '2' -> Two
    | _ -> failwith "Invalid card character"

let charToCard' =
    function
    | 'J' -> Joker
    | c -> charToCard c

type Hand =
    | FiveOfAKind
    | FourOfAKind
    | FullHouse
    | ThreeOfAKind
    | TwoPair
    | OnePair
    | HighCard

type HandData =
    { hand: Hand
      cards: Card list
      bid: int }

let (===) =
    function
    | [] -> true
    | x :: _ as xs -> List.forall ((=) x) xs

let identifyHand ((hand, bid): Card list * int) : HandData =
    let res =
        { hand = HighCard
          cards = hand
          bid = bid }

    match List.sort hand with
    | h when (===) h -> { res with hand = FiveOfAKind }
    | h when (===) (List.take 4 h) || (===) (List.skip 1 h) -> { res with hand = FourOfAKind }
    | h when (===) (List.take 2 h) && (===) (List.skip 2 h) -> { res with hand = FullHouse }
    | h when (===) (List.take 3 h) && (===) (List.skip 3 h) -> { res with hand = FullHouse }
    | h when (===) (List.take 3 h) -> { res with hand = ThreeOfAKind }
    | h when (===) (List.skip 2 h) -> { res with hand = ThreeOfAKind }
    | h when (===) (List.skip 1 h |> List.take 3) -> { res with hand = ThreeOfAKind }
    | [ a; b; c; d; _ ] when a = b && c = d -> { res with hand = TwoPair }
    | [ a; b; _; c; d ] when a = b && c = d -> { res with hand = TwoPair }
    | [ _; a; b; c; d ] when a = b && c = d -> { res with hand = TwoPair }
    | h when List.pairwise h |> List.exists (fun (a, b) -> a = b) -> { res with hand = OnePair }
    | _ -> res

let identifyHand' ((hand, bid): Card list * int) : HandData =
    let res =
        { hand = HighCard
          cards = hand
          bid = bid }

    match List.filter ((<>) Joker) hand |> List.sort with
    | h when (===) h -> { res with hand = FiveOfAKind }
    | [ _; _ ] -> { res with hand = FourOfAKind }
    | [ a; b; c ] when a = b || b = c -> { res with hand = FourOfAKind }
    | [ _; _; _ ] -> { res with hand = ThreeOfAKind }
    | [ a; b; c; d ] when (===) [ a; b; c ] || (===) [ b; c; d ] -> { res with hand = FourOfAKind }
    | [ a; b; c; d ] when a = b && c = d -> { res with hand = FullHouse }
    | [ a; b; c; d ] when a = b || b = c || c = d -> { res with hand = ThreeOfAKind }
    | [ _; _; _; _ ] -> { res with hand = OnePair }
    | _ -> identifyHand (hand, bid)

let parse =
    System.IO.File.ReadAllLines
    >> Seq.cast<string>
    >> Seq.map (fun (hand: string) ->
        match hand.Split(" ") with
        | [| hand'; bid |] when String.length hand' = 5 -> hand', int bid
        | _ -> failwith "invalid hand format")

let solve charFn handFn =
    Seq.map ((fun (hand, bid) -> Seq.map charFn hand |> Seq.toList, bid) >> handFn)
    >> Seq.sortByDescending (fun hand -> hand.hand, hand.cards)
    >> Seq.mapi (fun i hand -> (i + 1) * hand.bid)
    >> Seq.sum

let partOne = solve charToCard identifyHand
let partTwo = solve charToCard' identifyHand'

let run path =
    let parsed = parse path
    (partOne parsed, partTwo parsed)
