module aoc2021.Day10

open aoc2021.Common
open FSharp.Stats

let private testData =
    "[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]"
    |> readTestLines

let private matches (a: char) (b: char) : bool =
    (a = '{' && b = '}')
    || (a = '(' && b = ')')
    || (a = '[' && b = ']')
    || (a = '<' && b = '>')

let private isOpening (a: char) : bool = (set [ '{'; '('; '['; '<' ]).Contains a

let matching (c: char) : char =
    match c with
    | '{' -> '}'
    | '(' -> ')'
    | '[' -> ']'
    | '<' -> '>'
    | _ -> failwith "shouldn't happen"

type private CheckResult =
    | Incomplete of char list
    | NonMatching of char
    | OK

let private checkLine (s: string) : CheckResult =
    let rec r (stack: char list) (s: char list) : CheckResult =
        match s, stack with
        | x :: xs, _ when isOpening x -> r (x :: stack) xs
        | x :: xs, sx :: sxs ->
            if matches sx x then
                r sxs xs
            else
                NonMatching x
        | [], [] -> OK
        | [], _ :: _ -> Incomplete(stack |> List.map matching)
        | _ -> failwith "shouldn't happen"

    r List.empty (Seq.toList s)

let private cost1 (c: char) : int =
    match c with
    | ')' -> 3
    | ']' -> 57
    | '}' -> 1197
    | '>' -> 25137
    | _ -> failwith "unknown character"

let private solve1 (input: seq<string>) : int =
    input
    |> Seq.map checkLine
    |> Seq.choose
        (fun x ->
            match x with
            | NonMatching c -> Some c
            | _ -> None)
    |> Seq.map cost1
    |> Seq.sum

let test10_1 () : int = testData |> solve1
let solution10_1 () : int = readLines "day10.txt" |> solve1

let private cost2 (c: char) : int =
    match c with
    | ')' -> 1
    | ']' -> 2
    | '}' -> 3
    | '>' -> 4
    | _ -> failwith "unknown character"

let private totalCost2 (xs: char list) : bigint =
    xs
    |> List.fold (fun acc i -> acc * 5I + bigint (cost2 i)) 0I

let private solve2 (input: seq<string>) =
    input
    |> Seq.map checkLine
    |> Seq.choose
        (fun x ->
            match x with
            | Incomplete s -> Some s
            | _ -> None)
    |> Seq.map totalCost2
    |> Seq.sort
    |> median

let test10_2 () = testData |> solve2
let solution10_2 () = readLines "day10.txt" |> solve2
