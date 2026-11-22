module WorkoutNotes.Core.Tests.ScoringTests

open Xunit
open WorkoutNotes.Core
open WorkoutNotes.Core.Scoring

let legend: Legend =
    [ { Symbol = '*'; Label = "Ground"; Behaviour = NoHighs }
      { Symbol = '#'; Label = "Paused"; Behaviour = LabelOnly }
      { Symbol = '!'; Label = "Failed"; Behaviour = ExcludeFromCount } ]
    |> List.map (fun m -> m.Symbol, m)
    |> Map.ofList

let row reps =
    { Exercise = "Incline"
      Weight = Load 85m<kg>
      Reps = reps
      Note = None }

[<Fact>]
let ``epley matches the log's numbers`` () =
    Assert.Equal(102.0m<kg>, epley 85m<kg> 6)
    Assert.Equal(101.75m<kg>, epley 92.5m<kg> 3)

[<Fact>]
let ``ground reps don't set the best e1RM`` () =
    let r = row [ { Count = 5; Mark = None }; { Count = 7; Mark = Some '*' } ]
    Assert.Equal(Some(epley 85m<kg> 5), bestE1rm legend r)

[<Fact>]
let ``failed reps are out of volume, ground and paused are in`` () =
    let reps =
        [ { Count = 5; Mark = Some '!' }; { Count = 5; Mark = Some '*' }; { Count = 5; Mark = Some '#' } ]

    Assert.Equal<bool list>([ false; true; true ], reps |> List.map (countsForVolume legend))

[<Fact>]
let ``undefined marks are reported`` () =
    let r = row [ { Count = 5; Mark = Some '~' }; { Count = 5; Mark = Some '*' } ]
    Assert.Equal<Set<char>>(set [ '~' ], undefinedMarks legend [ r ])
