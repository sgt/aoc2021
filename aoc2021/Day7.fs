module aoc2021.Day7

open FSharp.Stats
open aoc2021.Common

let private readInput (input: seq<string>) : seq<int> =
    let s = input |> Seq.exactlyOne

    s.Split ',' |> Seq.map int

let private testData = "16,1,2,0,4,2,7,1,2,14" |> Seq.singleton

let private solve1 (input: seq<int>) : int =
    let pos = median input

    input
    |> Seq.map (fun x -> abs (x - pos))
    |> Seq.sum

let private crabFuel (distance: int) : int = distance * (distance + 1) / 2

let private solve2 (input: seq<int>) : int =

    let a = input |> Seq.toArray

    seq { Array.min a .. Array.max a }
    |> Seq.map (fun p ->
        a
        |> Array.map (fun x -> abs (x - p) |> crabFuel)
        |> Array.sum)
    |> Seq.min

let test7_1 () = testData |> readInput |> solve1

let solution7_1 () =
    readLines "day7.txt" |> readInput |> solve1

let test7_2 () = testData |> readInput |> solve2

let solution7_2 () =
    readLines "day7.txt" |> readInput |> solve2
