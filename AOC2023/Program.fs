// For more information see https://aka.ms/fsharp-console-apps

open Days

let printAnswers day =
    printfn "Running day %d...\n    part one -> %d\n    part two -> %d\n" day

let printAnswers' day =
    printfn "Running day %d...\n    part one -> %d\n    part two -> %d\n" day

[<EntryPoint>]
let main _ =
    printfn "Running all Advent of Code puzzles...\n"

    Day01.run "Data/Day01Input.txt" ||> printAnswers 1
    Day02.run "Data/Day02Input.txt" ||> printAnswers 2
    Day03.run "Data/Day03Input.txt" ||> printAnswers 3
    Day04.run "Data/Day04Input.txt" ||> printAnswers 4
    Day05.run "Data/Day05Input.txt" ||> printAnswers' 5
    // day 6
    Day07.run "Data/Day07Input.txt" ||> printAnswers 7

    0
