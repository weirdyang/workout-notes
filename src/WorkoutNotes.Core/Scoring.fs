module WorkoutNotes.Core.Scoring

let private behaviourOf (legend: Legend) (rep: Rep) =
    rep.Mark
    |> Option.bind (fun mark -> Map.tryFind mark legend)
    |> Option.map (fun definition -> definition.Behaviour)

let countsForVolume (legend: Legend) (rep: Rep) =
    behaviourOf legend rep <> Some ExcludeFromCount

let countsForHighs (legend: Legend) (rep: Rep) =
    match behaviourOf legend rep with
    | Some NoHighs
    | Some ExcludeFromCount -> false
    | Some LabelOnly
    | None -> true

let epleyOneRepMax (weight: decimal) (reps: int) : decimal =
    if reps <= 0 then 0.0m else weight * (1.0m + decimal reps / 30.0m)

let bestE1rm (legend: Legend) (row: ExerciseRow) : decimal option =
    match row.Weight with
    | Load(weight, _) ->
        row.Reps
        |> List.filter (countsForHighs legend)
        |> List.map (fun rep -> epleyOneRepMax weight rep.Count)
        |> function
            | [] -> None
            | estimates -> Some(List.max estimates)
    | _ -> None

let undefinedMarks (legend: Legend) (rows: ExerciseRow seq) =
    rows
    |> Seq.collect (fun row -> row.Reps)
    |> Seq.choose (fun rep -> rep.Mark)
    |> Seq.filter (fun mark -> not (Map.containsKey mark legend))
    |> Set.ofSeq
