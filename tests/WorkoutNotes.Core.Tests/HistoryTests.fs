module WorkoutNotes.Core.Tests.HistoryTests

open System
open Xunit
open WorkoutNotes.Core
open WorkoutNotes.Core.History

let legend: Legend =
    Map.ofList [ '*', { Symbol = '*'; Label = "Ground"; Behaviour = NoHighs } ]

let note (text: string) =
    match Parser.parseNote text with
    | Ok n -> n
    | Error e -> failwith e

let notes =
    [ "a", note "2026-11-14 Sat\n\nIncline @ 92.5x2\nIncline @ 85x5,5,5\nIncline @ 75x9"
      "b", note "2026-11-21 Sat\n\nIncline @ 92.5x3\nIncline @ 85x6,6,6\nIncline @ 75x9*\nincline  @ 85kg x 6\nSplit squat @ 90x5,5,5" ]

let all = rowsOf notes
let rowsIn id = all |> List.filter (fun r -> r.NoteId = id)

[<Fact>]
let ``badges on the second session`` () =
    let badges = rowsIn "b" |> List.map (badge legend all)
    Assert.Equal<Badge option list>([ Some RepHigh; Some RepHigh; None; Some Tie; None ], badges)

[<Fact>]
let ``first session has no badges`` () =
    Assert.True(rowsIn "a" |> List.forall (fun r -> badge legend all r = None))

[<Fact>]
let ``load high beats every earlier load`` () =
    let extra = note "2026-11-28 Sat\n\nIncline @ 95x1"
    let all = rowsOf (notes @ [ "c", extra ])
    let target = all |> List.find (fun r -> r.NoteId = "c")
    Assert.Equal(Some LoadHigh, badge legend all target)

[<Fact>]
let ``last session skips the current note`` () =
    match lastSession all "INCLINE" (DateOnly(2026, 11, 21)) "b" with
    | Some(date, rows) ->
        Assert.Equal(DateOnly(2026, 11, 14), date)
        Assert.Equal(3, rows.Length)
    | None -> failwith "expected a last session"

[<Fact>]
let ``bests by weight, heaviest first`` () =
    let bests = bestsByWeight legend all "Incline" |> List.map (fun (w, b, _) -> w, b)
    Assert.Equal<(Weight * int) list>([ Load(92.5m, "kg"), 3; Load(85m, "kg"), 6; Load(75m, "kg"), 9 ], bests)

[<Fact>]
let ``exercise names, latest first, case-insensitive`` () =
    Assert.Equal<string list>([ "Split squat"; "incline" ], exerciseNamesMostRecentFirst all)

[<Fact>]
let ``allBadges agrees with badge`` () =
    let extra = note "2026-11-28 Sat\n\nIncline @ 95x1\nIncline @ 95x1\nIncline @ 75x10*\nSplit squat @ 90x6,6\nSplit squat @ 92.5x3\nIncline @ 200lbx1\nIncline @ 210lbx1"
    let all = rowsOf (notes @ [ "c", extra ])
    let fast = allBadges legend all
    for r in all do
        Assert.Equal(badge legend all r, Map.tryFind (r.NoteId, r.Index) fast)

[<Fact>]
let ``loads only compare within a unit`` () =
    let extra = note "2026-11-28 Sat\n\nIncline @ 200lbx1\nIncline @ 210lbx1"
    let all = rowsOf (notes @ [ "c", extra ])
    let rows = all |> List.filter (fun r -> r.NoteId = "c")
    Assert.Equal<Badge option list>([ None; Some LoadHigh ], rows |> List.map (badge legend all))
