module aoc2021.Day8

open aoc2021.Common

let private testData =
    "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"
    |> readTestLines


let private parseSetsStr (s: string) : Set<char> [] =
    s.Split(' ')
    |> Array.map (fun x -> x.Trim())
    |> Array.filter ((<>) "")
    |> Array.map set

let private parseLine (line: string) : (Set<char> [] * Set<char> []) option =
    let parts = line.Split('|')

    if parts.Length <> 2 then
        None
    else
        let xs = parts |> Array.map parseSetsStr
        Some(xs.[0], xs.[1])

let private readInput (input: seq<string>) : seq<Set<char> [] * Set<char> []> = input |> Seq.choose parseLine

let private solve1 (input: seq<string>) : int =
    input
    |> readInput
    |> Seq.map
        (fun (_, digits) ->
            digits
            |> Array.filter (fun s -> List.contains s.Count [ 2; 4; 3; 7 ])
            |> Array.length)
    |> Seq.sum

let test8_1 () : int = testData |> solve1
let solution8_1 () : int = readLines "day8.txt" |> solve1

/// Determine signals for all digits based on input.
/// Should be done with constraint solving but can't be bothered.
let private determineSignals (inputs: Set<char> []) : Map<Set<char>, int> =
    let d1 =
        inputs |> Array.find (fun x -> x.Count = 2)

    let d4 =
        inputs |> Array.find (fun x -> x.Count = 4)

    let d7 =
        inputs |> Array.find (fun x -> x.Count = 3)

    let d8 =
        inputs |> Array.find (fun x -> x.Count = 7)

    let d3 =
        inputs
        |> Array.find (fun x -> x.Count = 5 && d1.IsSubsetOf x)

    let d6 =
        inputs
        |> Array.find
            (fun x ->
                let diff8 = d8 - x

                x.Count = 6 && diff8.IsSubsetOf d1)

    let d5 =
        inputs
        |> Array.find (fun x -> x.Count = 5 && (d6 - x).Count = 1)

    let d2 =
        inputs
        |> Array.find (fun x -> x.Count = 5 && x <> d3 && x <> d5)

    let d9 =
        inputs
        |> Array.find (fun x -> x.Count = 6 && (x - d3).Count = 1)

    let d0 =
        inputs
        |> Array.find (fun x -> x.Count = 6 && x <> d6 && x <> d3 && x <> d9)

    Map.ofList [ (d0, 0)
                 (d1, 1)
                 (d2, 2)
                 (d3, 3)
                 (d4, 4)
                 (d5, 5)
                 (d6, 6)
                 (d7, 7)
                 (d8, 8)
                 (d9, 9) ]

let private solve2 (input: seq<string>) : int =
    input
    |> readInput
    |> Seq.map
        (fun (signalsInput, digitsInput) ->
            let signals = determineSignals signalsInput

            digitsInput
            |> Array.map (fun x -> signals.[x])
            |> Array.fold (fun acc x -> acc * 10 + x) 0)
    |> Seq.sum

let test8_2 () : int = testData |> solve2
let solution8_2 () : int = readLines "day8.txt" |> solve2
