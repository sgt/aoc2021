module aoc2021.Day2

open aoc2021.Common

type Instruction =
    | Forward of int
    | Up of int
    | Down of int
    | Unknown

type Position =
    { Horizontal: int
      Depth: int
      Aim: int }

let private parse (s: string) : Instruction =
    let spl = s.Split(' ')

    if spl.Length < 2 then
        Unknown
    else
        let i = int spl.[1]

        match spl.[0] with
        | "forward" -> Forward i
        | "up" -> Up i
        | "down" -> Down i
        | _ -> Unknown

let private exec (pos: Position) (instr: Instruction) : Position =
    match instr with
    | Forward x ->
        { pos with
              Horizontal = pos.Horizontal + x }
    | Up x -> { pos with Depth = pos.Depth - x }
    | Down x -> { pos with Depth = pos.Depth + x }
    | Unknown -> pos

let private exec2 (pos: Position) (instr: Instruction) : Position =
    match instr with
    | Forward x ->
        { pos with
              Horizontal = pos.Horizontal + x
              Depth = pos.Depth + pos.Aim * x }
    | Up x -> { pos with Aim = pos.Aim - x }
    | Down x -> { pos with Aim = pos.Aim + x }
    | Unknown _ -> pos

let private solution (f: Position -> Instruction -> Position) : int =
    let initialPos: Position = { Horizontal = 0; Depth = 0; Aim = 0 }

    let finalPosition: Position =
        readLines "day2.txt"
        |> Seq.map parse
        |> Seq.fold f initialPos

    finalPosition.Horizontal * finalPosition.Depth

let solution2_1 () : int = solution exec
let solution2_2 () : int = solution exec2
