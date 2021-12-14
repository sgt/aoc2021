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
open aoc2021.Day10
open aoc2021.Day11
open aoc2021.Day12
open aoc2021.Day13

[<EntryPoint>]
let main argv =
    let day =
        if argv.Length > 0 then
            int argv.[0]
        else
            13

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
        printfn $"9.1: %A{solution9_1 ()}"
        printfn $"9.2: %A{solution9_2 ()}"
    | 10 ->
        printfn $"10.1: %A{solution10_1 ()}"
        printfn $"10.2: %A{solution10_2 ()}"
    | 11 ->
        printfn $"11.1: %A{solution11_1 ()}"
        printfn $"11.2: %A{solution11_2 ()}"
    | 12 ->
        printfn $"12.1: %A{solution12_1 ()}"
        printfn $"12.2: %A{test12_2_1 ()}"
        printfn $"12.2: %A{test12_2_2 ()}"
        printfn $"12.2: %A{test12_2_3 ()}"
        printfn $"12.2: %A{solution12_2 ()}"
    | 13 ->
        printfn $"13.1: %A{solution13_1 ()}"
        printfn $"13.2:\n%s{solution13_2 ()}"
    | _ -> failwith "no such day"

    0
