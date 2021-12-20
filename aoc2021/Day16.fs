module aoc2021.Day16

open System
open FSharpx.Collections
open Microsoft.FSharp.Core
open aoc2021.Common

let private testData = "" |> readTestLines

let private hexToBin (c: char) : string =
    match c with
    | '0' -> "0000"
    | '1' -> "0001"
    | '2' -> "0010"
    | '3' -> "0011"
    | '4' -> "0100"
    | '5' -> "0101"
    | '6' -> "0110"
    | '7' -> "0111"
    | '8' -> "1000"
    | '9' -> "1001"
    | 'A' -> "1010"
    | 'B' -> "1011"
    | 'C' -> "1100"
    | 'D' -> "1101"
    | 'E' -> "1110"
    | 'F' -> "1111"
    | _ -> failwith "unknown character"

type private PacketPayload =
    | Literal of int64
    | Operator of int * Packet list

and private Packet =
    { Version: int
      Payload: PacketPayload }

let rec private parseAll (bits: seq<char>) : seq<Packet> =
    seq {
        let packet, rest = parsePacket bits
        yield packet

        if rest |> Seq.isEmpty then
            yield! Seq.empty
        else
            yield! parseAll rest
    }

and private parseN (n: int) (bits: seq<char>) : seq<Packet> * seq<char> =
    if n = 0 then
        Seq.empty, bits
    else
        let packet, rest = parsePacket bits
        let prevPackets, rest' = parseN (n - 1) rest
        Seq.cons packet prevPackets, rest'

and private parseLiteral (bits: seq<char>) : PacketPayload * seq<char> =

    let rec loop (acc: seq<char> list) (xs: seq<char>) : PacketPayload * seq<char> =
        let chunk, rest = xs |> Seq.splitAt 5

        match chunk |> Seq.tryHeadTail with
        | Some ('1', data) -> loop (data :: acc) rest
        | Some ('0', data) ->
            let literalBits =
                data :: acc
                |> List.rev
                |> List.map List.ofSeq
                |> List.collect id
                |> String.Concat

            literalBits |> binToInt64 |> Literal, rest
        | _ -> failwith "error parsing literal"

    loop List.empty bits

and private parseOperator (t: int) (bits: seq<char>) : PacketPayload * seq<char> =
    match bits |> Seq.tryHeadTail with
    | Some ('0', rest) ->
        let bitLengthData, data = rest |> Seq.splitAt 15

        let bitLength =
            bitLengthData |> String.Concat |> binToInt

        let packetData, restData = data |> Seq.splitAt bitLength
        Operator(t, packetData |> parseAll |> List.ofSeq), restData

    | Some ('1', rest) ->
        let numPacketsData, data = rest |> Seq.splitAt 11

        let numPackets =
            numPacketsData |> String.Concat |> binToInt

        let packets, restData = data |> parseN numPackets
        Operator(t, packets |> List.ofSeq), restData

    | _ -> failwith "error parsing op mode"

and private parsePacket (bits: seq<char>) : Packet * seq<char> =
    let header, rawPayload = bits |> Seq.splitAt 6

    let version, packetType =
        header
        |> Seq.splitAt 3
        |> (fun (a, b) -> binSeqToInt a, binSeqToInt b)

    let payload, xs =
        match packetType with
        | 4 -> parseLiteral rawPayload
        | t -> parseOperator t rawPayload

    { Version = version; Payload = payload }, xs

let rec private sumPacket (packet: Packet) =
    match packet.Payload with
    | Literal _ -> packet.Version
    | Operator (_, xs) ->
        packet.Version
        + (xs |> List.map sumPacket |> List.sum)

let private parseInput (input: string) =
    input |> String.collect hexToBin |> parsePacket

let test16_1 () =
    "C0015000016115A2E0802F182340"
    |> parseInput
    |> fst
    |> sumPacket

let solution16_1 () =
    readLines "day16.txt"
    |> Seq.head
    |> parseInput
    |> fst
    |> sumPacket

let rec private interpret (packet: Packet) : int64 =
    match packet.Payload with
    | Literal x -> x |> int64
    | Operator (t, xs) ->
        let f: (int64 list -> int64) =
            match t with
            | 0 -> List.sum
            | 1 -> List.reduce (*)
            | 2 -> List.min
            | 3 -> List.max
            | 5 -> (fun xs -> if xs.Head > xs.Item 1 then 1L else 0L)
            | 6 -> (fun xs -> if xs.Head < xs.Item 1 then 1L else 0L)
            | 7 -> (fun xs -> if xs.Head = xs.Item 1 then 1L else 0L)
            | _ -> failwith $"unknown op type {t}"

        xs |> List.map interpret |> f

let test16_2 () =
    [ "C200B40A82"
      "04005AC33890"
      "880086C3E88112"
      "CE00C43D881120"
      "D8005AC2A8F0"
      "F600BC2D8F"
      "9C005AC2F8F0"
      "9C0141080250320F1802104A08" ]
    |> List.iter
        (fun x ->
            x
            |> parseInput
            |> fst
            |> interpret
            |> printfn "%A")

let solution16_2 () =
    readLines "day16.txt"
    |> Seq.head
    |> parseInput
    |> fst
    |> interpret
