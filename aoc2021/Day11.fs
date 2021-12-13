module aoc2021.Day11

open aoc2021.Common
open aoc2021.Grid

let private testData =
    "5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526"
    |> readTestLines

/// Returns state of grid after one step and set of positions flashed during that step.
let private step (grid: Grid<int>) : Grid<int> * Set<Position> =

    let rec processFlashes (grid: Grid<int>) (flashed: Set<Position>) : Grid<int> * Set<Position> =

        // determine who flashes this iteration
        let currentFlashes: Set<Position> =
            grid.Data
            |> Map.filter (fun pos v -> (not (flashed.Contains pos)) && v > 9)
            |> Map.keys
            |> set

        if currentFlashes.IsEmpty then
            // set all flashed to zero
            let flashedGrid =
                grid.map (fun pos v -> if flashed.Contains pos then 0 else v)
            // return from recursion
            flashedGrid, flashed
        else
            // determine neighbours of current flashes (who haven't flashed previously)
            // map position -> count (how many times appears as neighbour
            let neighboursCount =
                currentFlashes
                |> Seq.collect grid.neighbours
                |> Seq.map fst
                |> Seq.filter (fun x -> not (flashed.Contains x))
                |> Seq.countBy id
                |> Map.ofSeq

            // increase neighbours by their counts
            let increasedNeighboursGrid =
                grid.map
                    (fun pos v ->
                        v
                        + (neighboursCount.TryFind pos
                           |> Option.defaultValue 0))
            // recurse
            processFlashes increasedNeighboursGrid (flashed + currentFlashes)

    // increase whole grid by 1
    let increasedGrid = grid.map (fun _ v -> v + 1)
    let resultGrid, resultFlashed = processFlashes increasedGrid Set.empty
    //    printfn $"%s{resultGrid.ToString()}"
    resultGrid, resultFlashed

let private gridStatesSeq (input: seq<string>) : seq<Grid<int> * Set<Position>> =
    Seq.scan (fun (grid, _) _ -> step grid) ((input |> gridFromIntInput), Set.empty) (id |> Seq.initInfinite)


let private solve1 (iterations: int) (input: seq<string>) : int =
    let states =
        input
        |> gridStatesSeq
        |> Seq.take (iterations + 1)

    states
    |> Seq.map snd
    |> Seq.map Set.count
    |> Seq.sum

let test11_1 () : int = testData |> solve1 100
let solution11_1 () : int = readLines "day11.txt" |> solve1 100

let private solve2 (input: seq<string>) : int =
    input
    |> gridStatesSeq
    |> Seq.findIndex (fun (grid, _) -> grid.Data |> Map.forall (fun _ v -> v = 0))

let test11_2 () : int = testData |> solve2
let solution11_2 () : int = readLines "day11.txt" |> solve2
