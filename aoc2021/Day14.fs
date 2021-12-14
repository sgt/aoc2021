module aoc2021.Day14

open aoc2021.Common

let private testData =
    "NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C"
    |> readTestLines

type private RuleMap = Map<char * char, char>

type Template =
    { Pairs: Map<char * char, uint64>
      Start: char
      End: char }
    member this.letterCounts() : Map<char, uint64> =
        this.Pairs
        |> Map.fold
            (fun acc (c1, c2) v ->
                acc
                |> increaseMapCount c1 v
                |> increaseMapCount c2 v)
            Map.empty
        |> increaseMapCount this.Start 1UL
        |> increaseMapCount this.End 1UL
        |> Map.map (fun _ v -> v / 2UL)

let private readInput (input: seq<string>) : Template * RuleMap =
    let templateStr = input |> Seq.head

    let pairs =
        Seq.zip templateStr templateStr.[1..]
        |> Seq.countBy id
        |> Seq.map (fun (x, y) -> x, uint64 y)
        |> Map.ofSeq

    let rules =
        input
        |> Seq.skip 2
        |> Seq.choose
            (fun line ->
                match line with
                | Regex @"^(.)(.) -> (.)$" [ c1; c2; r ] -> Some((c1.[0], c2.[0]), r.[0])
                | _ -> None)
        |> Map.ofSeq

    { Pairs = pairs
      Start = templateStr.[0]
      End = templateStr.[templateStr.Length - 1] },
    rules

let private step (rules: RuleMap) (template: Template) : Template =
    let newPairs =
        template.Pairs
        |> Map.fold
            (fun acc (c1, c2 as k) v ->
                match rules.TryFind k with
                | Some cx ->
                    acc
                    |> increaseMapCount (c1, cx) v
                    |> increaseMapCount (cx, c2) v
                | None -> acc |> increaseMapCount k v)

            Map.empty

    { template with Pairs = newPairs }

let private solve1 (n: int) (template: Template, rules: RuleMap) : uint64 =
    let resultTemplate =
        { 1 .. n }
        |> Seq.fold (fun acc _ -> step rules acc) template

    let freqs =
        resultTemplate.letterCounts ()
        |> Map.toArray
        |> Array.sortByDescending snd

    (snd freqs.[0] - snd freqs.[freqs.Length - 1])

let test14_1 () = testData |> readInput |> solve1 10

let solution14_1 () =
    readLines "day14.txt" |> readInput |> solve1 10

let test14_2 () = testData |> readInput |> solve1 40

let solution14_2 () =
    readLines "day14.txt" |> readInput |> solve1 40
