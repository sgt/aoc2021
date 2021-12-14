module aoc2021.Day12

open System
open aoc2021.Common

let private testData1 =
    "start-A
start-b
A-c
A-b
b-d
A-end
b-end"
    |> readTestLines

let private testData2 =
    "dc-end
HN-start
start-kj
dc-start
dc-HN
LN-dc
HN-end
kj-sa
kj-HN
kj-dc"
    |> readTestLines

let private testData3 =
    "fs-end
he-DX
fs-he
start-DX
pj-DX
end-zg
zg-sl
zg-pj
pj-he
RW-he
fs-DX
pj-RW
zg-RW
start-pj
he-WI
zg-he
pj-fs
start-RW"
    |> readTestLines

type private Room =
    | Start
    | End
    | Large of string
    | Small of string

let private strToRoom (s: string) : Room =
    match s with
    | "start" -> Start
    | "end" -> End
    | _ when s.[0] |> Char.IsUpper -> Large s
    | _ when s.[0] |> Char.IsLower -> Small s
    | _ -> failwith $"unidentified room '{s}'"

type private Maze = list<Room * Room>

let private neighbours (maze: Maze) (node: Room) : Room list =
    maze
    |> List.filter (fun x -> fst x = node)
    |> List.map snd

let private parseMaze (input: seq<string>) : Maze =
    input
    |> Seq.choose
        (fun s ->
            let a = s.Split "-"

            if a.Length <> 2 then
                None
            else
                Some(a.[0], a.[1]))
    |> Seq.collect
        (fun (s1, s2) ->
            let r1, r2 = strToRoom s1, strToRoom s2
            [ (r1, r2); (r2, r1) ])
    |> Seq.toList

let private smallVisitedOnce (path: Room list) (room: Room) : bool = not (List.contains room path)

let private singleSmallVisitedTwice (path: Room list) (room: Room) : bool =
    let onlySmallRooms =
        path
        |> List.filter
            (fun r ->
                match r with
                | Small _ -> true
                | _ -> false)

    let anySmallVisitedTwice =
        onlySmallRooms
        |> List.countBy id
        |> List.tryFind (fun (_, cnt) -> cnt = 2)
        |> Option.isSome

    not (
        anySmallVisitedTwice
        && List.contains room onlySmallRooms
    )

let rec private paths (isSmallRoomEligibleFunc: Room list -> Room -> bool) (maze: Maze) : list<Room list> =
    let rec pathsStartingWith (path: Room list) =
        match path.Head with
        | End -> List.singleton (List.rev path)
        | lastRoom ->
            let eligibleNextRooms =
                neighbours maze lastRoom
                |> List.filter
                    (fun r ->
                        match r with
                        | Start -> false
                        | Large _ -> true
                        | Small _ -> isSmallRoomEligibleFunc path r
                        | End -> true)

            if eligibleNextRooms.IsEmpty then
                List.empty
            else
                List.collect (fun x -> pathsStartingWith (x :: path)) eligibleNextRooms

    pathsStartingWith (List.singleton Start)

let private solve (isSmallRoomEligibleFunc: Room list -> Room -> bool) (input: seq<string>) : int =
    input
    |> parseMaze
    |> paths isSmallRoomEligibleFunc
    |> List.length

let test12_1 () : int = testData3 |> solve smallVisitedOnce

let solution12_1 () : int =
    readLines "day12.txt" |> solve smallVisitedOnce

let test12_2_1 () : int =
    testData1 |> solve singleSmallVisitedTwice

let test12_2_2 () : int =
    testData2 |> solve singleSmallVisitedTwice

let test12_2_3 () : int =
    testData3 |> solve singleSmallVisitedTwice

let solution12_2 () : int =
    readLines "day12.txt"
    |> solve singleSmallVisitedTwice
