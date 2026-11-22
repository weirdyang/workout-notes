module WorkoutNotes.Core.History

open System

type DatedRow =
    { Date: DateOnly
      NoteId: string
      Index: int
      Row: ExerciseRow }

type Badge =
    | LoadHigh
    | RepHigh
    | Tie

let exerciseKey (name: string) =
    name.Trim().ToLower().Split([| ' '; '\t' |], StringSplitOptions.RemoveEmptyEntries)
    |> String.concat " "

let private chronologicalOrder (row: DatedRow) = row.Date, row.NoteId, row.Index

let rowsOf (notes: (string * Note) seq) : DatedRow list =
    let rowsOfNote (noteId: string, note: Note) =
        note.Lines
        |> Seq.mapi (fun index line ->
            match line with
            | Exercise row ->
                Some
                    { Date = note.Header.Date
                      NoteId = noteId
                      Index = index
                      Row = row }
            | _ -> None)
        |> Seq.choose id

    notes |> Seq.collect rowsOfNote |> Seq.sortBy chronologicalOrder |> List.ofSeq

let private sameExercise (a: DatedRow) (b: DatedRow) =
    exerciseKey a.Row.Exercise = exerciseKey b.Row.Exercise

let private isBefore (a: DatedRow) (b: DatedRow) =
    compare (chronologicalOrder a) (chronologicalOrder b) < 0

let private mostRepsCountingForHighs (legend: Legend) (row: ExerciseRow) =
    row.Reps
    |> List.filter (Scoring.countsForHighs legend)
    |> List.map (fun rep -> rep.Count)
    |> function
        | [] -> None
        | counts -> Some(List.max counts)

let private loadsInUnit (unit: string) (rows: DatedRow list) =
    rows
    |> List.choose (fun row ->
        match row.Row.Weight with
        | Load(value, rowUnit) when rowUnit = unit -> Some value
        | _ -> None)

let private compareWithBestReps (reps: int) (bestBefore: int) =
    if reps > bestBefore then Some RepHigh
    elif reps = bestBefore then Some Tie
    else None

let badge (legend: Legend) (all: DatedRow list) (target: DatedRow) : Badge option =
    let earlierRowsOfExercise =
        all |> List.filter (fun row -> sameExercise row target && isBefore row target)

    match mostRepsCountingForHighs legend target.Row with
    | None -> None
    | Some reps ->
        match target.Row.Weight with
        | Load(value, unit) when
            (let earlierLoads = loadsInUnit unit earlierRowsOfExercise
             not earlierLoads.IsEmpty && value > List.max earlierLoads)
            ->
            Some LoadHigh
        | weight ->
            let bestRepsAtThisWeight =
                earlierRowsOfExercise
                |> List.filter (fun row -> row.Row.Weight = weight)
                |> List.choose (fun row -> mostRepsCountingForHighs legend row.Row)

            match bestRepsAtThisWeight with
            | [] -> None
            | counts -> compareWithBestReps reps (List.max counts)

let allBadges (legend: Legend) (rowsOldestFirst: DatedRow list) : Map<string * int, Badge> =
    let mutable heaviestLoadSoFar: Map<string * string, decimal> = Map.empty
    let mutable mostRepsSoFar: Map<string * Weight, int> = Map.empty
    let mutable badges = Map.empty

    for row in rowsOldestFirst do
        let exercise = exerciseKey row.Row.Exercise

        match mostRepsCountingForHighs legend row.Row with
        | None -> ()
        | Some reps ->
            let badge =
                match row.Row.Weight with
                | Load(value, unit) when
                    (match Map.tryFind (exercise, unit) heaviestLoadSoFar with
                     | Some heaviest -> value > heaviest
                     | None -> false)
                    ->
                    Some LoadHigh
                | weight ->
                    Map.tryFind (exercise, weight) mostRepsSoFar
                    |> Option.bind (compareWithBestReps reps)

            badge |> Option.iter (fun b -> badges <- Map.add (row.NoteId, row.Index) b badges)

            let previousBest =
                Map.tryFind (exercise, row.Row.Weight) mostRepsSoFar |> Option.defaultValue -1

            mostRepsSoFar <- Map.add (exercise, row.Row.Weight) (max previousBest reps) mostRepsSoFar

        match row.Row.Weight with
        | Load(value, unit) ->
            let isHeavier =
                match Map.tryFind (exercise, unit) heaviestLoadSoFar with
                | Some heaviest -> value > heaviest
                | None -> true

            if isHeavier then
                heaviestLoadSoFar <- Map.add (exercise, unit) value heaviestLoadSoFar
        | _ -> ()

    badges

let lastSession (all: DatedRow list) (exercise: string) (onOrBefore: DateOnly) (excludingNoteId: string) =
    let key = exerciseKey exercise

    all
    |> List.filter (fun row ->
        exerciseKey row.Row.Exercise = key
        && row.NoteId <> excludingNoteId
        && row.Date <= onOrBefore)
    |> List.groupBy (fun row -> row.Date, row.NoteId)
    |> List.sortByDescending fst
    |> List.tryHead
    |> Option.map (fun ((date, _), rows) -> date, rows |> List.map (fun row -> row.Row))

let private heaviestFirst (weight: Weight) =
    match weight with
    | Load(value, unit) -> 3, unit, value
    | Added(value, unit) -> 2, unit, value
    | Bodyweight -> 1, "", 0m
    | Assisted(value, unit) -> 0, unit, -value
    | Unweighted -> -1, "", 0m

let bestsByWeight (legend: Legend) (all: DatedRow list) (exercise: string) =
    let key = exerciseKey exercise

    let bestAndFirstDateReached (weight: Weight, rows: DatedRow list) =
        let repsPerRow =
            rows
            |> List.choose (fun row -> mostRepsCountingForHighs legend row.Row |> Option.map (fun reps -> reps, row))

        match repsPerRow with
        | [] -> None
        | _ ->
            let best = repsPerRow |> List.map fst |> List.max
            let firstToReachBest = repsPerRow |> List.find (fun (reps, _) -> reps = best) |> snd
            Some(weight, best, firstToReachBest.Date)

    all
    |> List.filter (fun row -> exerciseKey row.Row.Exercise = key)
    |> List.groupBy (fun row -> row.Row.Weight)
    |> List.choose bestAndFirstDateReached
    |> List.sortByDescending (fun (weight, _, _) -> heaviestFirst weight)

let bestE1rm (legend: Legend) (all: DatedRow list) (exercise: string) =
    let key = exerciseKey exercise

    all
    |> List.filter (fun row -> exerciseKey row.Row.Exercise = key)
    |> List.choose (fun row -> Scoring.bestE1rm legend row.Row |> Option.map (fun e1rm -> e1rm, row))
    |> function
        | [] -> None
        | candidates -> Some(List.maxBy fst candidates)

let exerciseNamesMostRecentFirst (all: DatedRow list) =
    all
    |> List.rev
    |> List.distinctBy (fun row -> exerciseKey row.Row.Exercise)
    |> List.map (fun row -> row.Row.Exercise)
