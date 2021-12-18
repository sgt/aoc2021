module aoc2021.Day15

open FSharpx.Collections
open aoc2021.Common
open aoc2021.Grid

let private testData =
    "1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581"
    |> readTestLines

let private manhattanDistance ((x1, y1): Position) ((x2, y2): Position) = abs (x2 - x1) + abs (y2 - y1)

type private AStarState =
    { Fringe: Heap<int * Position>
      Closed: Set<Position>
      GScore: Map<Position, int>
      CameFrom: Map<Position, Position> }
    member this.unwindPath (start: Position) (goal: Position) : Position list =
        let rec loop (acc: Position list) (current: Position) : Position list =
            let path = current :: acc

            if current = start then
                path
            else
                loop path (this.CameFrom |> Map.find current)

        loop List.empty goal

let private shortestPath (grid: Grid<int>) (start: Position) (goal: Position) : Position list =

    let rec loop (state: AStarState) : Position list =
        // pop current node from fringe
        let (_, current), newFringe = state.Fringe |> Heap.uncons

        if current = goal then
            // retrace the path
            state.unwindPath start goal
        else
            let gCurrent = state.GScore |> Map.find current
            // neighbours not yet closed, whose g-scores better than already found
            let eligibleNeighboursWithGScores: (Position * int) list =
                grid.neighboursDirect current
                |> List.map (fun (p, v) -> (p, v + gCurrent))
                |> List.filter
                    (fun (p, g) ->
                        (state.Closed |> Set.contains p |> not)
                        && ((state.GScore |> Map.containsKey p |> not)
                            || (g < (state.GScore |> Map.find p))))
            // f-score -> position (to be merged with Fringe priority queue)
            let neighbourFScores: (int * Position) list =
                eligibleNeighboursWithGScores
                |> List.map (fun (p, g) -> (g + (manhattanDistance p goal), p))

            loop
                { Fringe =
                      neighbourFScores
                      |> Heap.ofSeq false
                      |> Heap.merge newFringe // neighbours' f-scores added
                  Closed = state.Closed |> Set.add current
                  GScore =
                      eligibleNeighboursWithGScores
                      |> Map.ofSeq
                      |> Map.union state.GScore // neighbours' g-scores added
                  CameFrom =
                      eligibleNeighboursWithGScores
                      |> List.map (fun (n, _) -> (n, current))
                      |> Map.ofSeq
                      |> Map.union state.CameFrom // all neighbours came from current
                }


    loop
        { Fringe = ([ (0, start) ] |> Heap.ofSeq false)
          Closed = Set.empty
          GScore = Map.ofSeq [ (start, 0) ]
          CameFrom = Map.empty }

let private solve (input: seq<string>) =
    let grid = input |> gridFromIntInput

    shortestPath grid (0, 0) (grid.Width - 1, grid.Height - 1)
    |> List.map (fun p -> p, grid.Data |> Map.find p)
    |> List.map snd
    |> List.tail
    |> List.sum

let test15_1 () = testData |> solve
let solution15_1 () = readLines "day15.txt" |> solve
