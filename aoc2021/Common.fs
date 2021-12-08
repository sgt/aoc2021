module aoc2021.Common

open System.IO
open System.Reflection
open System.Text.RegularExpressions

let readLines (dataFileName: string) : seq<string> =

    seq {
        let assembly = Assembly.GetExecutingAssembly()
        let resourceName = "aoc2021.data." + dataFileName

        use stream =
            assembly.GetManifestResourceStream(resourceName)

        use sr = new StreamReader(stream)

        while not sr.EndOfStream do
            yield sr.ReadLine().Trim()
    }

let readIntegers (dataFileName: string) : seq<int> = readLines dataFileName |> Seq.map int

let readTestLines (s: string) : seq<string> =
    s.Split '\n'
    |> Array.map (fun x -> x.Trim())
    |> Array.toSeq

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)

    if m.Success then
        Some(List.tail [ for g in m.Groups -> g.Value ])
    else
        None

/// for chain debugging
let tap data =
    let materialized = List.ofSeq data
    printfn $"%A{materialized}"
    data
