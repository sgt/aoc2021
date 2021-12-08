module aoc2021.Day6

open aoc2021.Common

let private readInput (input: seq<string>) : Map<int, uint64> =
    let s = input |> Seq.exactlyOne

    s.Split ','
    |> Array.map int
    |> Array.countBy id
    |> Array.map (fun (a, b) -> a, uint64 b)
    |> Map.ofArray

let private testData =
    "3,4,3,1,2" |> Seq.singleton |> readInput

let private resetCycle = 7
let private initCycle = 8

let private incOrSet (m: Map<'K, uint64>) (k: 'K, v: uint64) : Map<'K, uint64> =
    m
    |> Map.change
        k
        (fun o ->
            match o with
            | Some x -> Some(x + v)
            | None -> Some(v))

let private step (xs: Map<int, uint64>) : Map<int, uint64> =
    let fishSpawned = Option.defaultValue 0UL (Map.tryFind 0 xs)

    let oldFish =
        xs
        |> Map.toList
        |> List.map (fun (k, v) -> (if k = 0 then resetCycle - 1 else k - 1), v)
        |> List.fold incOrSet Map.empty

    if fishSpawned > 0UL then
        Map.add initCycle fishSpawned oldFish
    else
        oldFish

let private solve (days: int) (input: Map<int, uint64>) : uint64 =
    Seq.fold (fun acc _ -> step acc) input (seq { 1 .. days })
    |> Map.values
    |> Seq.sum

let test6_1 () = testData |> solve 80

let solution6_1 () =
    readLines "day6.txt" |> readInput |> solve 80

let test6_2 () = testData |> solve 256

let solution6_2 () =
    readLines "day6.txt" |> readInput |> solve 256
