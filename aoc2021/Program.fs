module aoc2021.Program

open aoc2021.Day1
open aoc2021.Day2
open aoc2021.Day3
open aoc2021.Day4
open aoc2021.Day5
open aoc2021.Day6
open aoc2021.Day7
open aoc2021.Day8
open aoc2021.Day9

[<EntryPoint>]
let main argv =
    let day =
        if argv.Length > 0 then
            int argv.[0]
        else
            9

    match day with
    | 1 ->
        printfn $"1.1: %A{solution1_1 ()}"
        printfn $"1.2: %A{solution1_2 ()}"
    | 2 ->
        printfn $"2.1: %A{solution2_1 ()}"
        printfn $"2.2: %A{solution2_2 ()}"
    | 3 ->
        printfn $"3.1: %A{solution3_1 ()}"
        printfn $"3.2: %A{solution3_2 ()}"
    | 4 ->
        printfn $"4.1: %A{solution4_1 ()}"
        printfn $"4.2: %A{solution4_2 ()}"
    | 5 ->
        printfn $"5.1: %A{solution5_1 ()}"
        printfn $"5.2: %A{solution5_2 ()}"
    | 6 ->
        printfn $"6.1: %A{solution6_1 ()}"
        printfn $"6.2: %A{solution6_2 ()}"
    | 7 ->
        printfn $"7.1: %A{solution7_1 ()}"
        printfn $"7.2: %A{solution7_2 ()}"
    | 8 ->
        printfn $"8.1: %A{solution8_1 ()}"
        printfn $"8.2: %A{solution8_2 ()}"
    | 9 ->
//        printfn $"9.1: %A{solution9_1 ()}"
        printfn $"9.2: %A{solution9_2 ()}"
    | _ -> failwith "no such day"

    0
