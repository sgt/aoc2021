module aoc2021.Day3

open aoc2021.Common

let private testData =
    readTestLines
        """00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010
"""
    |> Seq.filter (fun x -> x <> "")
    |> Seq.toList

let private flip (c: char) : char = if c = '1' then '0' else '1'

let private flipString (s: string) : string = String.map flip s

let private mostCommonElement (input: list<char>) : char =
    let ones: int =
        input
        |> List.filter (fun x -> x = '1')
        |> List.length

    let zeroes: int = (List.length input) - ones

    if ones >= zeroes then '1' else '0'

let private leastCommonElement (input: list<char>) : char = input |> mostCommonElement |> flip

let private toGamma (numbers: list<string>) : string =
    let len: int = numbers |> List.item 0 |> String.length

    seq { 0 .. (len - 1) }
    |> Seq.map (fun x -> List.map (fun (n: string) -> n.[x]) numbers)
    |> Seq.map mostCommonElement
    |> System.String.Concat

let private solve3_1 (input: list<string>) : int =
    let binGamma = input |> toGamma
    let binEpsilon = flipString binGamma
    binToInt binGamma * binToInt binEpsilon

let solution3_1 () : int =
    readLines "day3.txt" |> Seq.toList |> solve3_1

let test3_1 () : int = solve3_1 testData

let private filterByBit (f: list<char> -> char) (bitPos: int) (xs: list<string>) : list<string> =
    let digits: list<char> =
        List.map (fun (x: string) -> x.[bitPos]) xs

    List.filter (fun x -> x.[bitPos] = f digits) xs

let private findRating (f: list<char> -> char) (xs: list<string>) : string =
    let rec findRating' (f: list<char> -> char) (xs: list<string>) (pos: int) =
        let result = filterByBit f pos xs

        if (List.length result) = 1 then
            List.exactlyOne result
        else
            findRating' f result (pos + 1)

    findRating' f xs 0

let private solve3_2 (input: list<string>) : int =
    let oxygen = findRating mostCommonElement input
    let co2 = findRating leastCommonElement input
    binToInt oxygen * binToInt co2

let test3_2 () : int = solve3_2 testData

let solution3_2 () : int =
    readLines "day3.txt" |> Seq.toList |> solve3_2
