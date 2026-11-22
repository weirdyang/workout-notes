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
    [ "a", note "2026-11-14 Sat\n\n[Incline] 92.5 2\n[Incline] 85 5,5,5\n[Incline] 75 9"
      "b", note "2026-11-21 Sat\n\n[Incline] 92.5 3\n[Incline] 85 6,6,6\n[Incline] 75 9*\n[incline]  85 6\n[Split squat] 90 5,5,5" ]

let all = rowsOf notes
let rowsIn id = all |> List.filter (fun r -> r.NoteId = id)

[<Fact>]
let ``badges on the second session`` () =
    let badges = rowsIn "b" |> List.map (badge legend all)
    // 92.5 x3 > x2 → rep high; 85 x6 > x5 → rep high; 75 x9* doesn't count; incline 85 x6 ties the line above;
    // split squat is a first exposure → nothing
    Assert.Equal<Badge option list>([ Some RepHigh; Some RepHigh; None; Some Tie; None ], badges)

[<Fact>]
let ``first session has no badges`` () =
    Assert.True(rowsIn "a" |> List.forall (fun r -> badge legend all r = None))

[<Fact>]
let ``load high beats every earlier load`` () =
    let extra = note "2026-11-28 Sat\n\n[Incline] 95 1"
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
    Assert.Equal<(Weight * int) list>([ Load 92.5m<kg>, 3; Load 85m<kg>, 6; Load 75m<kg>, 9 ], bests)

[<Fact>]
let ``exercise names, latest first, case-insensitive`` () =
    Assert.Equal<string list>([ "Split squat"; "incline" ], exerciseNames all)

[<Fact>]
let ``allBadges agrees with badge`` () =
    let extra = note "2026-11-28 Sat\n\n[Incline] 95 1\n[Incline] 95 1\n[Incline] 75 10*\n[Split squat] 90 6,6\n[Split squat] 92.5 3"
    let all = rowsOf (notes @ [ "c", extra ])
    let fast = allBadges legend all
    for r in all do
        Assert.Equal(badge legend all r, Map.tryFind (r.NoteId, r.Index) fast)
