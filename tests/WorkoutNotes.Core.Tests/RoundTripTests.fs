module WorkoutNotes.Core.Tests.RoundTripTests

open FsCheck
open FsCheck.Xunit
open WorkoutNotes.Core

let private nameChars =
    [ 'a' .. 'z' ] @ [ 'A' .. 'Z' ] @ [ '0' .. '9' ] @ [ ' '; '-'; '·'; '/'; '+'; '*' ]

let private genName =
    Gen.elements nameChars
    |> Gen.listOf
    |> Gen.map (fun cs -> System.String(List.toArray cs).Trim())
    |> Gen.filter (fun s -> s <> "")

let private genKg =
    Gen.choose (0, 40000) |> Gen.map (fun n -> decimal n / 100m * 1.0m<kg>)

let private genWeight =
    Gen.oneof
        [ genKg |> Gen.map Load
          genKg |> Gen.map Added
          genKg |> Gen.map Assisted
          Gen.constant Bodyweight
          Gen.constant Unweighted ]

let private genRep =
    Gen.map2
        (fun n m -> { Count = n; Mark = m })
        (Gen.choose (0, 100))
        (Gen.oneof [ Gen.constant None; Gen.elements [ '*'; '#'; '!'; '^'; '~' ] |> Gen.map Some ])

let private genNote =
    Gen.oneof
        [ Gen.constant None
          Gen.elements ([ 'a' .. 'z' ] @ [ ' '; '/'; '*'; '[' ])
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

[<Property(Arbitrary = [| typeof<Generators> |], MaxTest = 1000)>]
let ``print then parse returns the same row`` (row: ExerciseRow) =
    let printed = Printer.printRow row
    Parser.parseLine printed = Exercise row |@ printed

[<Property(Arbitrary = [| typeof<Generators> |])>]
let ``parseLine never throws`` (s: string) =
    let s = if isNull s then "" else s
    Parser.parseLine s |> ignore
    true
