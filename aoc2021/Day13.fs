module aoc2021.Day13

open System
open aoc2021.Common

let private testData =
    "6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5"
    |> readTestLines

type private Position = int * int

type private Fold =
    | X of int
    | Y of int

type private Paper = Set<Position>

let private parseInput (input: seq<string>) : Paper * Fold list =
    let paper =
        input
        |> Seq.choose
            (fun line ->
                match line with
                | Regex @"^(\d+),(\d+)$" [ x; y ] -> Some(int x, int y)
                | _ -> None)
        |> set

    let folds =
        input
        |> Seq.choose
            (fun line ->
                match line with
                | Regex @"^fold along (.)=(\d+)$" [ axis; num ] ->
                    match axis with
                    | "x" -> Some(X(int num))
                    | "y" -> Some(Y(int num))
                    | _ -> None
                | _ -> None)
        |> Seq.toList

    (paper, folds)

let private flipX (column: int) (paper: Paper) : Paper =
    paper
    |> Set.map (fun (x, y) -> (column - x - 1, y))

let private flipY (row: int) (paper: Paper) : Paper =
    // row 7 -> (0,6)->(0,0) ; (0,0)->(0,6)
    paper |> Set.map (fun (x, y) -> (x, row - y - 1))

let private splitX (column: int) (paper: Paper) : Paper * Paper =
    Set.fold
        (fun (p1, p2) (x, y) ->
            if x = column then (p1, p2)
            elif x < column then (p1.Add(x, y), p2)
            else (p1, p2.Add(x - column - 1, y)))
        (Set.empty, Set.empty)
        paper

let private splitY (row: int) (paper: Paper) : Paper * Paper =
    Set.fold
        (fun (p1, p2) (x, y) ->
            if y = row then (p1, p2)
            elif y < row then (p1.Add(x, y), p2)
            else (p1, p2.Add(x, y - row - 1)))
        (Set.empty, Set.empty)
        paper

let private paperToString (paper: Paper) : string =
    let maxX = paper |> Set.map fst |> Set.maxElement
    let maxY = paper |> Set.map snd |> Set.maxElement

    String.concat
        "\n"
        (seq [ for y in 0 .. maxY do
                   String.Concat [ for x in 0 .. maxX do
                                       yield
                                           if paper.Contains((x, y)) then
                                               '#'
                                           else
                                               '.' ] ])

let private solve (paper: Paper) (folds: Fold list) : Paper =
    List.fold
        (fun acc i ->
            match i with
            | X col ->
                let p1, p2 = splitX col acc
                p1 + (flipX col p2)
            | Y row ->
                let p1, p2 = splitY row acc
                p1 + (flipY row p2))
        paper
        folds

let private solve1 (paper: Paper, folds: Fold list) : int =
    solve paper (List.take 1 folds)
    |> Set.count
let private solve2 (paper: Paper, folds: Fold list) : string =
    solve paper folds
    |> paperToString
let test13_1 () = testData |> parseInput |> solve1

let solution13_1 () =
    readLines "day13.txt" |> parseInput |> solve1

let test13_2 () = testData |> parseInput|>solve2
let solution13_2 () =
    readLines "day13.txt" |> parseInput |> solve2
