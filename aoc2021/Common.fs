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
let tap (data: 'a) : 'a =
    printfn $"%A{data}"
    data

let superCountBy (projection: 'T -> 'Key) (source: seq<'T>) : seq<'Key * uint64> =
    source
    |> Seq.fold
        (fun acc i -> Map.change (projection i) (fun o -> Some((Option.defaultValue 0UL o) + 1UL)) acc)
        Map.empty
    |> Map.toSeq

let increaseMapCount (key: 'Key) (count: uint64) (table: Map<'Key, uint64>) =
    table
    |> Map.change
        key
        (fun o ->
            match o with
            | Some x -> Some(x + count)
            | None -> Some(count))

let binToInt (s: string) : int =
    s.ToCharArray()
    |> Array.fold (fun acc x -> acc * 2 + (x |> string |> int)) 0

let binToInt64 (s: string) : int64 =
    s.ToCharArray()
    |> Array.fold (fun acc x -> acc * 2L + (x |> string |> int64)) 0

let binSeqToInt (s: seq<char>) : int =
    s
    |> Seq.fold (fun acc x -> acc * 2 + (x |> string |> int)) 0
