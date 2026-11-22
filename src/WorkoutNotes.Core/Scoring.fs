module WorkoutNotes.Core.Scoring

/// Reps that count toward volume, given the user's legend. Unknown marks count (they're flagged elsewhere).
let countsForVolume (legend: Legend) (r: Rep) =
    match r.Mark |> Option.bind (fun m -> Map.tryFind m legend) with
    | Some { Behaviour = ExcludeFromCount } -> false
    | _ -> true

/// Reps allowed to set or tie a rep high.
let countsForHighs (legend: Legend) (r: Rep) =
    match r.Mark |> Option.bind (fun m -> Map.tryFind m legend) with
    | Some { Behaviour = NoHighs }
    | Some { Behaviour = ExcludeFromCount } -> false
    | _ -> true

/// Epley: weight × (1 + reps/30).
let epley (w: decimal<kg>) (reps: int) : decimal<kg> =
    if reps <= 0 then 0.0m<kg> else w * (1.0m + decimal reps / 30.0m)

/// Best e1RM in a row, using only reps that count for highs. Only plain loads have an e1RM.
let bestE1rm (legend: Legend) (row: ExerciseRow) : decimal<kg> option =
    match row.Weight with
    | Load w ->
        row.Reps
        |> List.filter (countsForHighs legend)
        |> List.map (fun r -> epley w r.Count)
        |> function
            | [] -> None
            | xs -> Some(List.max xs)
    | _ -> None

/// Marks used in rows that the legend doesn't define.
let undefinedMarks (legend: Legend) (rows: ExerciseRow seq) =
    rows
    |> Seq.collect (fun r -> r.Reps)
    |> Seq.choose (fun r -> r.Mark)
    |> Seq.filter (fun m -> not (Map.containsKey m legend))
    |> Set.ofSeq
