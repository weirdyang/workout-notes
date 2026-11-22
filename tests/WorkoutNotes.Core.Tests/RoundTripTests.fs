module WorkoutNotes.Core.Tests.RoundTripTests

open FsCheck
open FsCheck.Xunit
open WorkoutNotes.Core

let private nameChars =
    [ 'a' .. 'z' ] @ [ 'A' .. 'Z' ] @ [ '0' .. '9' ] @ [ ' '; '-'; '·'; '/'; '+'; '*'; '['; ']'; 'x' ]

let private genName =
    Gen.elements nameChars
    |> Gen.listOf
    |> Gen.map (fun cs -> System.String(List.toArray cs).Trim())
    |> Gen.filter (fun s -> s <> "")

let private genUnit = Gen.elements [ "kg"; "lb"; "plates"; "units" ]

let private genAmount =
    Gen.map2 (fun n u -> decimal n / 100m, u) (Gen.choose (0, 40000)) genUnit

let private genWeight =
    Gen.oneof
        [ genAmount |> Gen.map Load
          genAmount |> Gen.map Added
          genAmount |> Gen.map Assisted
          Gen.constant Bodyweight
          Gen.constant Unweighted ]

let private genRep =
    Gen.map2
        (fun n m -> { Count = n; Mark = m })
        (Gen.choose (0, 100))
        (Gen.oneof [ Gen.constant None; Gen.elements [ '*'; '#'; '!'; '^'; '~'; 'w' ] |> Gen.map Some ])

let private genNote =
    Gen.oneof
        [ Gen.constant None
          Gen.elements ([ 'a' .. 'z' ] @ [ ' '; '/'; '*'; '['; '@' ])
          |> Gen.listOf
          |> Gen.map (fun cs -> System.String(List.toArray cs).Trim())
          |> Gen.map (fun s -> if s = "" then None else Some s) ]

let private genRow =
    gen {
        let! name = genName
        let! weight = genWeight
        let! reps = Gen.nonEmptyListOf genRep
        let! note = genNote

        return
            { Exercise = name
              Weight = weight
              Reps = reps
              Note = note }
    }

type Generators =
    static member Row() = Arb.fromGen genRow

[<Property(Arbitrary = [| typeof<Generators> |], MaxTest = 2000)>]
let ``print then parse returns the same row`` (row: ExerciseRow) =
    let printed = Printer.printRow row
    Parser.parseLine printed = Exercise row |@ printed

[<Property(Arbitrary = [| typeof<Generators> |], MaxTest = 500)>]
let ``tidy is idempotent`` (row: ExerciseRow) =
    let once = Printer.tidyLine (Printer.printRow row)
    Printer.tidyLine once = once

[<Property>]
let ``parseLine never throws`` (s: string) =
    let s = if isNull s then "" else s
    Parser.parseLine s |> ignore
    true
