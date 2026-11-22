module WorkoutNotes.Core.Tests.ParserTests

open System
open Xunit
open WorkoutNotes.Core
open WorkoutNotes.Core.Parser

let row name weight reps note =
    Exercise
        { Exercise = name
          Weight = weight
          Reps = reps
          Note = note }

let r n = { Count = n; Mark = None }
let rm n m = { Count = n; Mark = Some m }

[<Theory>]
[<InlineData("[Front squat]    72.5   8,8,7*")>]
[<InlineData("[Front squat] 72.5 8, 8, 7*")>]
[<InlineData("  [ Front squat ] 72.5 8,8,7*  ")>]
let ``spec example: front squat`` (line: string) =
    Assert.Equal(row "Front squat" (Load 72.5m<kg>) [ r 8; r 8; rm 7 '*' ] None, parseLine line)

[<Fact>]
let ``added weight`` () =
    Assert.Equal(row "Pull-up" (Added 16m<kg>) [ r 4; r 4; rm 3 '#' ] None, parseLine "[Pull-up] +16 4,4,3#")

[<Fact>]
let ``assisted weight, ascii and unicode minus`` () =
    let expected = row "Assisted dip" (Assisted 18m<kg>) [ r 10; r 10 ] None
    Assert.Equal(expected, parseLine "[Assisted dip] -18 10,10")
    Assert.Equal(expected, parseLine "[Assisted dip] −18 10,10")

[<Fact>]
let ``bodyweight`` () =
    Assert.Equal(row "Hanging leg raise" Bodyweight [ r 20; r 20; r 20 ] None, parseLine "[Hanging leg raise] BW 20,20,20")

[<Fact>]
let ``no weight: a lone number is reps`` () =
    Assert.Equal(row "Low row" Unweighted [ r 12; r 12 ] None, parseLine "[Low row] 12,12")
    Assert.Equal(row "Plank" Unweighted [ r 3 ] None, parseLine "[Plank] 3")

[<Fact>]
let ``names with digits and dots`` () =
    Assert.Equal(row "3STPBS" (Load 70m<kg>) [ r 3; r 3; r 3 ] None, parseLine "[3STPBS] 70 3,3,3")
    Assert.Equal(row "Hamstring curl · long-ROM" (Load 72.5m<kg>) [ r 8; r 8 ] None, parseLine "[Hamstring curl · long-ROM] 72.5 8,8")

[<Fact>]
let ``trailing note`` () =
    Assert.Equal(row "Incline" (Load 92.5m<kg>) [ r 3 ] (Some "moved smooth"), parseLine "[Incline] 92.5 3 // moved smooth")
    Assert.Equal(row "Incline" (Load 92.5m<kg>) [ r 3 ] (Some "x"), parseLine "[Incline] 92.5 3// x")

[<Fact>]
let ``weight then note with no reps is unparsed, not mangled`` () =
    match parseLine "[Incline] 92.5 // forgot reps" with
    | Unparsed(raw, _) -> Assert.Equal("[Incline] 92.5 // forgot reps", raw)
    | other -> failwithf "expected Unparsed, got %A" other

[<Theory>]
[<InlineData("[Incline 92.5 3")>]
[<InlineData("[] 92.5 3")>]
[<InlineData("[Incline]")>]
[<InlineData("[Incline] 92.5 3,")>]
[<InlineData("[Incline] 92.5 3,,4")>]
[<InlineData("[Incline] 92.5 3 extra")>]
[<InlineData("[Incline] 92.5 3+")>]
[<InlineData("[Incline] 92.5 3.")>]
let ``bad rows are kept as Unparsed`` (line: string) =
    match parseLine line with
    | Unparsed(raw, err) ->
        Assert.Equal(line, raw)
        Assert.False(String.IsNullOrWhiteSpace err)
    | other -> failwithf "expected Unparsed, got %A" other

[<Fact>]
let ``free text and blanks`` () =
    Assert.Equal(Line.Text "legs cooked by the end", parseLine "  legs cooked by the end ")
    Assert.Equal(Blank, parseLine "   ")

[<Fact>]
let ``note with header`` () =
    let text =
        "2026-11-21 Sat · Incline + legs\n\n[Incline] 92.5 3\n[Incline] 85 6,6,6\nlegs cooked by the end\n"

    match parseNote text with
    | Error e -> failwith e
    | Ok note ->
        Assert.Equal(DateOnly(2026, 11, 21), note.Header.Date)
        Assert.Equal(Some "Incline + legs", note.Header.Title)

        Assert.Equal<Line list>(
            [ row "Incline" (Load 92.5m<kg>) [ r 3 ] None
              row "Incline" (Load 85m<kg>) [ r 6; r 6; r 6 ] None
              Line.Text "legs cooked by the end"
              Blank ],
            note.Lines
        )

[<Fact>]
let ``header without title, and bad date`` () =
    Assert.Equal(Ok { Date = DateOnly(2026, 11, 22); Title = None }, parseHeader "2026-11-22")
    Assert.True(Result.isError (parseHeader "22 Nov 2026 · Run"))

[<Theory>]
[<InlineData("Incline 85 6,6,6", "[Incline] 85 6,6,6")>]
[<InlineData("Low bar Smith 105 10", "[Low bar Smith] 105 10")>]
[<InlineData("3STPBS 70 3,3,3", "[3STPBS] 70 3,3,3")>]
[<InlineData("Pull-up +16 4,4,3#", "[Pull-up] +16 4,4,3#")>]
[<InlineData("Hanging leg raise BW 20,20,20", "[Hanging leg raise] BW 20,20,20")>]
[<InlineData("Incline 92.5 3 // moved smooth", "[Incline] 92.5 3 // moved smooth")>]
[<InlineData("legs cooked by the end", "legs cooked by the end")>]
[<InlineData("did 3 sets", "did 3 sets")>]
[<InlineData("[Incline] 85 6", "[Incline] 85 6")>]
[<InlineData("85 6,6,6", "85 6,6,6")>]
let ``normalizeLine wraps the exercise name`` (input: string, expected: string) =
    Assert.Equal(expected, normalizeLine input)
