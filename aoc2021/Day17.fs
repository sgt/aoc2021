module aoc2021.Day17

open System

(*
Part 1: figured out on paper: y speed should be (abs(minY)-1), the highest point is speed*(speed+1)/2
*)

let private solveQuadratic a b c =
    let disc = b * b - 4.0 * a * c
    (-b - sqrt disc) / (2.0 * a), (-b + sqrt disc) / (2.0 * a)

let private naturalsBetween a b : seq<int> =
    let f1, f2 = if a > b then b, a else a, b
    seq { int (ceil f1) .. int (floor f2) }

type private TargetArea =
    { X: int * int
      Y: int * int }
    member this.minYVelocity = this.Y |> fst
    member this.maxYVelocity = -(this.Y |> fst) + 1

    member this.minXVelocity =
        let minRoots =
            solveQuadratic 0.5 0.5 (-(this.X |> fst |> float))

        let maxRoots =
            solveQuadratic 0.5 0.5 (-(this.X |> snd |> float))

        let minRoot = max (fst minRoots) (snd minRoots)
        let maxRoot = max (fst maxRoots) (snd maxRoots)
        let speeds = naturalsBetween minRoot maxRoot

        if speeds |> Seq.isEmpty then
            this.X |> fst
        else
            speeds |> Seq.min

    member this.maxXVelocity = this.X |> snd
    member this.isWithinX n = n >= (fst this.X) && n <= (snd this.X)

let private testData = { X = (20, 30); Y = (-10, -5) }

let private problemData = { X = (111, 161); Y = (-154, -101) }

let private stepsWithYHits (initYVelocity: int) (target: TargetArea) =
    // [a=-0.5 ; b=initY+0.5 ; c = -y]
    let minY, maxY = target.Y

    let minRoots =
        solveQuadratic -0.5 (float initYVelocity + 0.5) -minY

    let maxRoots =
        solveQuadratic -0.5 (float initYVelocity + 0.5) -maxY

    naturalsBetween (fst minRoots) (fst maxRoots)
    |> Seq.append (naturalsBetween (snd minRoots) (snd maxRoots))
    |> Seq.filter (fun x -> x >= 0)

let getXPos (step: int) (initVelocity: int) =
    if step >= initVelocity then
        initVelocity * (initVelocity + 1) / 2
    else
        (initVelocity * (initVelocity + 1) / 2)
        - (initVelocity - step) * (initVelocity - step + 1)
          / 2

let private xVelocitiesForStep (step: int) (target: TargetArea) : seq<int> =
    [ for v in seq { target.minXVelocity .. target.maxXVelocity } do
          let pos = getXPos step v
          if target.isWithinX pos then v ]

let private solve2 (target: TargetArea) =
    let velocitiesToNumberOfSteps =
        seq { target.minYVelocity .. target.maxYVelocity }
        |> Seq.map (fun v -> (v, (stepsWithYHits v target |> Seq.toList)))

    [ for yv, steps in velocitiesToNumberOfSteps do
          for s in steps do
              for xv in xVelocitiesForStep s target do
                  xv, yv ]
    |> set

let test17_2 () = solve2 testData |> Set.count
let solution17_2 () = solve2 problemData |> Set.count
