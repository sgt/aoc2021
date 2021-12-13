module aoc2021.Day9

open aoc2021.Common
open aoc2021.Grid

let private testData =
    "2199943210
3987894921
9856789892
8767896789
9899965678"
    |> readTestLines


let private isLow (grid: Grid<_>) (pos: Position) : bool =
    let v = grid.Data.[pos]

    grid.neighboursDirect pos
    |> List.map snd
    |> List.forall (fun i -> (i > v))

let private lows (grid: Grid<int>) : seq<int> =
    grid.Data
    |> Map.filter (fun pos _ -> isLow grid pos)
    |> Map.values
    |> seq

let private solution1 (input: seq<string>) : int =
    input
    |> gridFromIntInput
    |> lows
    |> Seq.map ((+) 1)
    |> Seq.sum

let test9_1 () : int = testData |> solution1
let solution9_1 () : int = readLines "day9.txt" |> solution1

let private discoverBasinSeed (grid: Grid<_>) (found: Set<Position>) : Position option =
    grid.Data
    |> Map.tryFindKey (fun pos v -> v <> 9 && not (Set.contains pos found))

let private discoverBasin (grid: Grid<_>) (seed: Position) : Set<Position> =
    let rec discoverBasinRec (found: Set<Position>) (queue: Set<Position>) : Set<Position> =
        if queue.IsEmpty then
            found
        else
            let init = queue.MinimumElement

            let newlyFound =
                grid.neighboursDirect init
                |> List.filter (fun (pos, v) -> v <> 9 && not (found.Contains pos))
                |> List.map fst
                |> set

            discoverBasinRec (found.Add init) ((queue.Remove init) + newlyFound)

    discoverBasinRec Set.empty (Set.singleton seed)

let private discoverAllBasins (grid: Grid<_>) : Set<Position> list =
    let rec discoverAllBasinsRec (acc: Set<Position> list) : Set<Position> list =
        let found = acc |> List.fold (+) Set.empty

        match discoverBasinSeed grid found with
        | None -> acc
        | Some seed -> discoverAllBasinsRec ((discoverBasin grid seed) :: acc)

    discoverAllBasinsRec List.empty

let solution2 (input: seq<string>) : int =
    input
    |> gridFromIntInput
    |> discoverAllBasins
    |> List.map Set.count
    |> List.sortDescending
    |> List.take 3
    |> List.reduce (*)

let test9_2 () : int = testData |> solution2
let solution9_2 () : int = readLines "day9.txt" |> solution2
