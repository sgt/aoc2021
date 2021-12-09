module aoc2021.Day9

open aoc2021.Common

let private testData =
    "2199943210
3987894921
9856789892
8767896789
9899965678"
    |> readTestLines

type private Position = int * int

type private Board(input: seq<string>) =
    let data =
        input
        |> Seq.toArray
        |> Array.map (Seq.toArray >> Array.map (string >> int))

    let height = data.Length
    let width = data.[0].Length

    member this.get(x: int, y: int) : int = data.[y].[x]

    member this.left(x: int, y: int) : (Position * int) option =
        if x = 0 then
            None
        else
            let pos = ((x - 1), y)
            Some(pos, this.get pos)

    member this.right(x: int, y: int) : (Position * int) option =
        if x = width - 1 then
            None
        else
            let pos = ((x + 1), y)
            Some(pos, this.get pos)

    member this.up(x: int, y: int) : (Position * int) option =
        if y = 0 then
            None
        else
            let pos = (x, (y - 1))
            Some(pos, this.get pos)

    member this.down(x: int, y: int) : (Position * int) option =
        if y = height - 1 then
            None
        else
            let pos = (x, (y + 1))
            Some(pos, this.get pos)

    member this.isLow(pos: Position) : bool =
        let v = this.get pos

        [ this.left pos
          this.right pos
          this.up pos
          this.down pos ]
        |> List.choose (Option.map snd)
        |> List.forall (fun i -> (i > v))

    // a seq with all cells in board as position * value
    member this.cells: seq<Position * int> =
        seq {
            for y in 0 .. height - 1 do
                for x in 0 .. width - 1 do
                    yield ((x, y), this.get (x, y))
        }

    member this.lows: seq<int> =
        [ for pos, v in this.cells do
              if this.isLow pos then v ]

let private solution1 (input: seq<string>) : int =
    Board(input).lows |> Seq.map ((+) 1) |> Seq.sum

let test9_1 () : int = testData |> solution1
let solution9_1 () : int = readLines "day9.txt" |> solution1

let private discoverBasinSeed (board: Board) (found: Set<Position>) : Position option =
    board.cells
    |> Seq.tryFind (fun (pos, v) -> not (Set.contains pos found) && v <> 9)
    |> Option.map fst

let private discoverBasin (board: Board) (seed: Position) : Set<Position> =
    let rec discoverBasinRec (found: Set<Position>) (queue: Set<Position>) : Set<Position> =
        if queue.IsEmpty then
            found
        else
            let init = queue.MinimumElement

            let newlyFound =
                [ board.up init
                  board.down init
                  board.left init
                  board.right init ]
                |> List.choose id
                |> List.filter (fun (pos, v) -> v <> 9 && not (found.Contains pos))
                |> List.map fst
                |> set

            discoverBasinRec (found.Add init) ((queue.Remove init) + newlyFound)

    discoverBasinRec Set.empty (Set.singleton seed)

let private discoverAllBasins (board: Board) : Set<Position> list =
    let rec discoverAllBasinsRec (acc: Set<Position> list) : Set<Position> list =
        let found = acc |> List.fold (+) Set.empty

        match discoverBasinSeed board found with
        | None -> acc
        | Some seed -> discoverAllBasinsRec ((discoverBasin board seed) :: acc)

    discoverAllBasinsRec List.empty

let solution2 (input: seq<string>) : int =
    discoverAllBasins (Board(input))
    |> List.map Set.count
    |> List.sortDescending
    |> List.take 3
    |> List.reduce (*)

let test9_2 () : int = testData |> solution2
let solution9_2 () : int = readLines "day9.txt" |> solution2
