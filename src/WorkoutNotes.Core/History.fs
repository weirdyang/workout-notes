module WorkoutNotes.Core.History

open System

/// A parsed row with where it came from. Order = Date, then NoteId, then Index (line order).
type DatedRow =
    { Date: DateOnly
      NoteId: string
      Index: int
      Row: ExerciseRow }

type Badge =
    | LoadHigh
    | RepHigh
    | Tie

/// Exercise identity for comparison: case- and whitespace-insensitive name.
let exerciseKey (name: string) =
    name.Trim().ToLower().Split([| ' '; '\t' |], StringSplitOptions.RemoveEmptyEntries)
    |> String.concat " "

let private order (r: DatedRow) = r.Date, r.NoteId, r.Index

/// All exercise rows across notes, oldest first.
let rowsOf (notes: (string * Note) seq) : DatedRow list =
    notes
    |> Seq.collect (fun (id, note) ->
        note.Lines
        |> Seq.mapi (fun i line -> i, line)
        |> Seq.choose (fun (i, line) ->
            match line with
            | Exercise row ->
                Some
                    { Date = note.Header.Date
                      NoteId = id
                      Index = i
                      Row = row }
            | _ -> None))
    |> Seq.sortBy order
    |> List.ofSeq

let private sameExercise (a: DatedRow) (b: DatedRow) =
    exerciseKey a.Row.Exercise = exerciseKey b.Row.Exercise

let private isBefore (a: DatedRow) (b: DatedRow) = compare (order a) (order b) < 0

/// Best rep count in a row among reps the legend allows for highs.
let private bestReps (legend: Legend) (row: ExerciseRow) =
    row.Reps
    |> List.filter (Scoring.countsForHighs legend)
    |> List.map (fun r -> r.Count)
    |> function
        | [] -> None
        | xs -> Some(List.max xs)

/// Badge for one row, against every earlier row of the same exercise (earlier notes and earlier lines).
/// First exposure of an exercise or a weight gets no badge: there is nothing to beat.
let badge (legend: Legend) (all: DatedRow list) (target: DatedRow) : Badge option =
    let priors =
        all |> List.filter (fun r -> sameExercise r target && isBefore r target)

    match bestReps legend target.Row with
    | None -> None
    | Some reps ->
        let priorLoads =
            priors
            |> List.choose (fun r ->
                match r.Row.Weight with
                | Load w -> Some w
                | _ -> None)

        match target.Row.Weight with
        | Load w when not priorLoads.IsEmpty && w > List.max priorLoads -> Some LoadHigh
        | weight ->
            let atWeight =
                priors
                |> List.filter (fun r -> r.Row.Weight = weight)
                |> List.choose (fun r -> bestReps legend r.Row)

            match atWeight with
            | [] -> None
            | xs ->
                let best = List.max xs

                if reps > best then Some RepHigh
                elif reps = best then Some Tie
                else None

/// Badges for every row in one pass (same rules as `badge`), keyed by (NoteId, Index).
/// `rows` must be oldest first, as `rowsOf` returns them.
let allBadges (legend: Legend) (rows: DatedRow list) : Map<string * int, Badge> =
    let mutable maxLoad: Map<string, decimal<kg>> = Map.empty
    let mutable bestAt: Map<string * Weight, int> = Map.empty
    let mutable result = Map.empty

    for r in rows do
        let key = exerciseKey r.Row.Exercise

        match bestReps legend r.Row with
        | None -> ()
        | Some reps ->
            let b =
                match r.Row.Weight, Map.tryFind key maxLoad with
                | Load w, Some m when w > m -> Some LoadHigh
                | weight, _ ->
                    match Map.tryFind (key, weight) bestAt with
                    | Some best when reps > best -> Some RepHigh
                    | Some best when reps = best -> Some Tie
                    | _ -> None

            b |> Option.iter (fun b -> result <- Map.add (r.NoteId, r.Index) b result)

            let prev = Map.tryFind (key, r.Row.Weight) bestAt |> Option.defaultValue -1
            bestAt <- Map.add (key, r.Row.Weight) (max prev reps) bestAt

        // a row with no countable reps still counts as a load done (matches `badge`, which uses all prior loads)
        match r.Row.Weight with
        | Load w ->
            let prev = Map.tryFind key maxLoad
            if prev.IsNone || w > prev.Value then maxLoad <- Map.add key w maxLoad
        | _ -> ()

    result

/// The most recent earlier session (another note) that has this exercise: its date and rows.
let lastSession (all: DatedRow list) (exercise: string) (beforeDate: DateOnly) (excludeNote: string) =
    let key = exerciseKey exercise

    all
    |> List.filter (fun r ->
        exerciseKey r.Row.Exercise = key
        && r.NoteId <> excludeNote
        && r.Date <= beforeDate)
    |> List.groupBy (fun r -> r.Date, r.NoteId)
    |> List.sortByDescending fst
    |> List.tryHead
    |> Option.map (fun ((date, _), rows) -> date, rows |> List.map (fun r -> r.Row))

/// Best reps ever at each weight, heaviest first, with the date it was done.
let bestsByWeight (legend: Legend) (all: DatedRow list) (exercise: string) =
    let key = exerciseKey exercise

    all
    |> List.filter (fun r -> exerciseKey r.Row.Exercise = key)
    |> List.groupBy (fun r -> r.Row.Weight)
    |> List.choose (fun (weight, rows) ->
        rows
        |> List.choose (fun r -> bestReps legend r.Row |> Option.map (fun b -> b, r))
        |> function
            | [] -> None
            | xs ->
                // highest reps; earliest date that reached it
                let best = xs |> List.map fst |> List.max
                let first = xs |> List.find (fun (b, _) -> b = best) |> snd
                Some(weight, best, first.Date))
    |> List.sortByDescending (fun (w, _, _) ->
        match w with
        | Load d
        | Added d -> decimal d
        | Assisted d -> -(decimal d)
        | Bodyweight
        | Unweighted -> 0m)

/// Best e1RM across all rows of an exercise, with the row it came from.
let bestE1rm (legend: Legend) (all: DatedRow list) (exercise: string) =
    let key = exerciseKey exercise

    all
    |> List.filter (fun r -> exerciseKey r.Row.Exercise = key)
    |> List.choose (fun r -> Scoring.bestE1rm legend r.Row |> Option.map (fun e -> e, r))
    |> function
        | [] -> None
        | xs -> Some(List.maxBy fst xs)

/// Distinct exercise names, most recently used first (display name = latest spelling).
let exerciseNames (all: DatedRow list) =
    all
    |> List.rev
    |> List.distinctBy (fun r -> exerciseKey r.Row.Exercise)
    |> List.map (fun r -> r.Row.Exercise)
