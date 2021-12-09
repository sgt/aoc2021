module aoc2021.Day5

open aoc2021.Common

let testData =
    "0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2"
    |> readTestLines

let private sign (n: int) : int =
    if n < 0 then -1
    elif n > 0 then 1
    else 0

let rec private gcd x y = if y = 0 then x else gcd y (x % y)

type Vent =
    { X1: int
      Y1: int
      X2: int
      Y2: int }
    member v.isVerticalOrHorizontal: bool = v.X1 = v.X2 || v.Y1 = v.Y2
    member v.xLen: int = v.X2 - v.X1
    member v.yLen = v.Y2 - v.Y1

    member v.allPoints: seq<int * int> =
        let xRange, yRange =
            if v.xLen = 0 then
                seq {
                    while true do
                        v.X1
                },
                seq { v.Y1 .. sign v.yLen .. v.Y2 }
            elif v.yLen = 0 then
                seq { v.X1 .. sign v.xLen .. v.X2 },
                seq {
                    while true do
                        v.Y1
                }
            else
                let denom = gcd v.xLen v.yLen |> abs
                let xStep, yStep = v.xLen / denom, v.yLen / denom
                seq { v.X1 .. xStep .. v.X2 }, seq { v.Y1 .. yStep .. v.Y2 }

        Seq.zip xRange yRange


let readVents (input: seq<string>) : seq<Vent> =
    input
    |> Seq.choose
        (fun x ->
            match x with
            | Regex @"^(\d+),(\d+) -> (\d+),(\d+)$" [ x1; y1; x2; y2 ] ->
                Some
                    { X1 = int x1
                      Y1 = int y1
                      X2 = int x2
                      Y2 = int y2 }
            | _ -> None)

let private solve (onlyVerticalOrHorizontal: bool) (input: seq<string>) : int =
    input
    |> readVents
    |> Seq.filter
        (fun v ->
            not onlyVerticalOrHorizontal
            || v.isVerticalOrHorizontal)
    |> Seq.collect (fun v -> v.allPoints)
    |> Seq.countBy id
    |> Seq.filter (fun x -> snd x > 1)
    |> Seq.length

let test5_1 () : int = testData |> solve true
let solution5_1 () : int = readLines "day5.txt" |> solve true
let test5_2 () : int = testData |> solve false
let solution5_2 () : int = readLines "day5.txt" |> solve false
