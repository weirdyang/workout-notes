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
let kg v = Load(v, "kg")

[<Theory>]
[<InlineData("incline@85x3,3,3")>]
[<InlineData("incline @ 85x3,3,3")>]
[<InlineData("incline @ 85 x 3,3,3")>]
[<InlineData("incline @85 3,3,3")>]
[<InlineData("incline @ 85kgx3,3,3")>]
[<InlineData("incline @ 85kg x 3, 3, 3")>]
[<InlineData("incline @ 85KGx3,3,3")>]
[<InlineData("incline @ 85kgs x3,3,3")>]
[<InlineData("  incline   @85x3,3,3  ")>]
[<InlineData("incline @ 85×3,3,3")>]
let ``same row, many spellings`` (line: string) =
    Assert.Equal(row "incline" (kg 85m) [ r 3; r 3; r 3 ] None, parseLine line)

[<Fact>]
let ``units`` () =
    Assert.Equal(row "Curl" (Load(40m, "lb")) [ r 10 ] None, parseLine "Curl @ 40lbx10")
    Assert.Equal(row "Curl" (Load(40m, "lb")) [ r 10 ] None, parseLine "Curl @ 40lbs 10")
    Assert.Equal(row "Farmer walk" (Load(2m, "plates")) [ r 40; r 40 ] None, parseLine "Farmer walk @ 2platesx40,40")
    Assert.Equal(row "Front squat" (kg 72.5m) [ r 8; r 8; rm 7 '*' ] None, parseLine "Front squat @ 72.5x8,8,7*")

[<Fact>]
let ``added, assisted, bodyweight, none`` () =
    Assert.Equal(row "Pull-up" (Added(16m, "kg")) [ r 4; r 4; rm 3 '#' ] None, parseLine "Pull-up @ +16x4,4,3#")
    Assert.Equal(row "Assisted dip" (Assisted(18m, "kg")) [ r 10; r 10 ] None, parseLine "Assisted dip @ -18x10,10")
    Assert.Equal(row "Assisted dip" (Assisted(18m, "kg")) [ r 10 ] None, parseLine "Assisted dip @ −18 x 10")
    Assert.Equal(row "Leg raise" Bodyweight [ r 20; r 20 ] None, parseLine "Leg raise @ bw 20,20")
    Assert.Equal(row "Leg raise" Bodyweight [ r 20 ] None, parseLine "Leg raise @ BWx20")
    Assert.Equal(row "Plank" Unweighted [ r 60; r 60 ] None, parseLine "Plank @ - 60,60")
    Assert.Equal(row "Plank" Unweighted [ r 60; r 60 ] None, parseLine "Plank @ 60,60")

[<Fact>]
let ``names can hold digits, symbols, x and spaces`` () =
    Assert.Equal(row "3STPBS" (kg 70m) [ r 3; r 3; r 3 ] None, parseLine "3STPBS @ 70x3,3,3")
    Assert.Equal(row "Hamstring curl · long-ROM" (kg 72.5m) [ r 8; r 8 ] None, parseLine "Hamstring curl · long-ROM @ 72.5x8,8")
    Assert.Equal(row "Box jump" (kg 0m) [ r 5 ] None, parseLine "Box jump @ 0x5")

[<Fact>]
let ``trailing note, which may contain @`` () =
    Assert.Equal(row "Incline" (kg 92.5m) [ r 3 ] (Some "moved smooth"), parseLine "Incline @ 92.5x3 // moved smooth")
    Assert.Equal(row "Incline" (kg 92.5m) [ r 3 ] (Some "ask @coach"), parseLine "Incline @ 92.5x3// ask @coach")

[<Theory>]
[<InlineData("@ 85x3")>]
[<InlineData("incline @")>]
[<InlineData("incline @ 85x")>]
[<InlineData("incline @ 85x3,")>]
[<InlineData("incline @ 85x3,,4")>]
[<InlineData("incline @ 85x3 extra")>]
[<InlineData("incline @ 85x3+")>]
[<InlineData("incline @ 85x3.")>]
[<InlineData("incline @ 85x3x5")>]
[<InlineData("email me@example.com")>]
let ``lines with @ that aren't rows are kept as Unparsed`` (line: string) =
    match parseLine line with
    | Unparsed(raw, err) ->
        Assert.Equal(line, raw)
        Assert.False(String.IsNullOrWhiteSpace err)
    | other -> failwithf "expected Unparsed, got %A" other

[<Theory>]
[<InlineData("legs cooked by the end")>]
[<InlineData("did 3 sets")>]
[<InlineData("incline 85 6,6,6")>]
let ``lines without @ are text`` (line: string) =
    Assert.Equal(Line.Text line, parseLine line)

[<Fact>]
let ``old bracket rows are still read`` () =
    Assert.Equal(row "Front squat" (kg 72.5m) [ r 8; r 8; rm 7 '*' ] None, parseLine "[Front squat] 72.5 8,8,7*")
    Assert.Equal(row "Pull-up" (Added(16m, "kg")) [ r 4 ] None, parseLine "[Pull-up] +16 4")
    Assert.Equal(row "Low row" Unweighted [ r 12; r 12 ] None, parseLine "[Low row] 12,12")
    Assert.Equal(Line.Text "[not a row", parseLine "[not a row")

[<Fact>]
let ``free text and blanks`` () =
    Assert.Equal(Line.Text "legs cooked by the end", parseLine "  legs cooked by the end ")
    Assert.Equal(Blank, parseLine "   ")

[<Fact>]
let ``note with header`` () =
    let text = "2026-11-21 Sat · Incline + legs\n\nIncline @ 92.5x3\nIncline @ 85x6,6,6\nlegs cooked by the end\n"

    match parseNote text with
    | Error e -> failwith e
    | Ok note ->
        Assert.Equal(DateOnly(2026, 11, 21), note.Header.Date)
        Assert.Equal(Some "Incline + legs", note.Header.Title)

        Assert.Equal<Line list>(
            [ row "Incline" (kg 92.5m) [ r 3 ] None
              row "Incline" (kg 85m) [ r 6; r 6; r 6 ] None
              Line.Text "legs cooked by the end"
              Blank ],
            note.Lines
        )

[<Theory>]
[<InlineData("2026-11-22 Sun · Push", "Push")>]
[<InlineData("2026-11-22 push", "push")>]
[<InlineData("2026-11-22 Sun push day", "push day")>]
[<InlineData("2026-11-22 Sunday long run", "Sunday long run")>]
[<InlineData("2026-11-22 Sun", "")>]
[<InlineData("2026-11-22", "")>]
[<InlineData("2026-11-22 Sun · ", "")>]
let ``header titles`` (line: string, title: string) =
    match parseHeader line with
    | Ok h ->
        Assert.Equal(DateOnly(2026, 11, 22), h.Date)
        Assert.Equal((if title = "" then None else Some title), h.Title)
    | Error e -> failwith e

[<Fact>]
let ``bad date`` () =
    Assert.True(Result.isError (parseHeader "22 Nov 2026 · Run"))
    Assert.True(Result.isError (parseHeader "2026-02-30 · Run"))

[<Theory>]
[<InlineData("incline@85x3,3,3", "incline @ 85x3,3,3")>]
[<InlineData("Curl @ 40lbs x 10", "Curl @ 40lbx10")>]
[<InlineData("Leg raise @ BW x 20, 20", "Leg raise @ bw 20,20")>]
[<InlineData("Plank @ 60,60", "Plank @ - 60,60")>]
[<InlineData("[Front squat] 72.5 8,8,7*", "Front squat @ 72.5x8,8,7*")>]
[<InlineData("did 3 sets", "did 3 sets")>]
[<InlineData("incline @ 85x", "incline @ 85x")>]
let ``tidyLine`` (input: string, expected: string) =
    Assert.Equal(expected, Printer.tidyLine input)
