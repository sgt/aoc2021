module aoc2021.Day1

open aoc2021.Common

let private increases (data: seq<int * int>) : int =
    data
    |> Seq.map (fun (x, y) -> y - x)
    |> Seq.filter (fun x -> x > 0)
    |> Seq.length

let private pairs_by_sums_of_triplets (data: seq<int>) : seq<int * int> =
    Seq.zip3 data (Seq.skip 1 data) (Seq.skip 2 data)
    |> Seq.map (fun (x, y, z) -> x + y + z)
    |> Seq.pairwise

let solution1_1 () : int =
    readIntegers "day1.txt"
    |> Seq.pairwise
    |> increases

let solution1_2 () : int =
    readIntegers "day1.txt"
    |> pairs_by_sums_of_triplets
    |> increases
