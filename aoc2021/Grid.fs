module aoc2021.Grid

type Position = int * int

type Grid<'T> =
    { Data: Map<Position, 'T>
      Width: int
      Height: int }

    member private this.findNeighbours(positions: Position list) : (Position * 'T) list =
        positions
        |> List.filter
            (fun (x, y) ->
                x >= 0
                && x < this.Width
                && y >= 0
                && y < this.Height)
        |> List.map (fun pos -> pos, this.Data.[pos])

    /// only up, down, left, right
    member this.neighboursDirect(x: int, y: int) : (Position * 'T) list =
        [ (x - 1, y)
          (x + 1, y)
          (x, y - 1)
          (x, y + 1) ]
        |> this.findNeighbours

    /// direct + diagonals
    member this.neighbours(x: int, y: int) : (Position * 'T) list =
        List.concat [ this.neighboursDirect (x, y)
                      [ (x - 1, y - 1)
                        (x - 1, y + 1)
                        (x + 1, y - 1)
                        (x + 1, y + 1) ]
                      |> this.findNeighbours ]

    member this.map(f: Position -> 'T -> 'U) : Grid<'U> =
        { Width = this.Width
          Height = this.Height
          Data = Map.map f this.Data }

    override this.ToString() : string =
        (seq { 0 .. this.Height - 1 })
        |> Seq.map
            (fun y ->
                (seq { 0 .. this.Width - 1 })
                |> Seq.map (fun x -> string this.Data.[(x, y)])
                |> String.concat "")
        |> String.concat "\n"

let gridFromIntInput (input: seq<string>) : Grid<int> =
    let data =
        input
        |> Seq.toArray
        |> Array.map (Seq.toArray >> Array.map (string >> int))

    let height = data.Length
    let width = data.[0].Length

    let b =
        seq {
            for y in 0 .. height - 1 do
                for x in 0 .. width - 1 do
                    yield ((x, y), data.[y].[x])
        }
        |> Map.ofSeq

    { Data = b
      Width = width
      Height = height }
