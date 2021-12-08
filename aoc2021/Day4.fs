module aoc2021.Day4

open System
open aoc2021.Common

type Cell = bool * int
type Board = Cell [] []

let testData: seq<string> =
    "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7"
    |> readTestLines

let private readBingoInput (input: seq<string>) : int [] * Board [] =
    let inputArray = Seq.toArray input

    let numbers: int [] =
        inputArray.[0].Split ',' |> Array.map int

    let boards: Board [] =
        inputArray
        |> Array.skip 1
        |> Array.filter ((<>) "")
        |> Array.map (fun s -> s.Split(' ', StringSplitOptions.RemoveEmptyEntries))
        |> Array.map (Array.map (fun x -> false, int x))
        |> Array.chunkBySize 5

    (numbers, boards)

let private checkRow (row: Cell []) : bool = Array.forall fst row

let private hasWon (board: Board) : bool =
    Array.exists
        checkRow
        (Array.concat [ board
                        (Array.transpose board) ])

// TODO mark can also check for victory for optimisation
let private mark (n: int) (board: Board) : Board =
    board
    |> Array.map (Array.map (fun (m, i) -> if i = n then (true, i) else (m, i)))

// state = optional winningBoard, last_used_number, current boards
type State = Board option * int * Board []

// TODO only worth tracking boards that still haven't won for optimisation
let private step ((_, _, boards): State) (num: int) : State =
    let priorWinners = Array.map hasWon boards
    let newBoards = Array.map (mark num) boards
    let newWinners = Array.map hasWon newBoards

    let control =
        Array.zip3 priorWinners newWinners newBoards

    let optWinningTriplet =
        control
        |> Array.tryFind (fun (p, n, _) -> not p && n)

    let optWinner =
        optWinningTriplet
        |> Option.map (fun (_, _, b) -> b)

    (optWinner, num, newBoards)

/// sum of unmarked cells
let private score (board: Board) : int =
    Array.sum (
        board
        |> Array.map
            (fun (row: Cell []) ->
                row
                |> Array.filter (not << fst)
                |> Array.map snd
                |> Array.sum)
    )

let private solve (forward: bool) (input: seq<string>) : int =
    let numbers, boards = input |> readBingoInput

    let findFunc =
        if forward then
            Seq.find
        else
            Seq.findBack

    let winningBoard, winningNumber, _ =
        // state = last_used_number, current boards
        Seq.scan step (None, 0, boards) numbers
        |> findFunc (fun (o, _, _) -> o.IsSome)

    winningNumber * score winningBoard.Value

let test4_1 () : int = solve true testData

let solution4_1 () : int = readLines "day4.txt" |> solve true

let test4_2 () : int = solve false testData

let solution4_2 () : int = readLines "day4.txt" |> solve false
