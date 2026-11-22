module WorkoutNotes.Core.Printer

open System.Globalization

let private fmt (d: decimal<kg>) =
    (decimal d).ToString("0.###", CultureInfo.InvariantCulture)

let printWeight =
    function
    | Load d -> Some(fmt d)
    | Added d -> Some("+" + fmt d)
    | Assisted d -> Some("-" + fmt d)
    | Bodyweight -> Some "BW"
    | Unweighted -> None

let printRep (r: Rep) =
    match r.Mark with
    | Some m -> $"{r.Count}{m}"
    | None -> string r.Count

let printRow (r: ExerciseRow) =
    [ yield $"[{r.Exercise}]"
      match printWeight r.Weight with
      | Some w -> yield w
      | None -> ()
      yield r.Reps |> List.map printRep |> String.concat ","
      match r.Note with
      | Some n -> yield $"// {n}"
      | None -> () ]
    |> String.concat " "

let printLine =
    function
    | Exercise r -> printRow r
    | Text t -> t
    | Blank -> ""
    | Unparsed(raw, _) -> raw

let printHeader (h: Header) =
    let date = h.Date.ToString("yyyy-MM-dd ddd", CultureInfo.InvariantCulture)

    match h.Title with
    | Some t -> $"{date} · {t}"
    | None -> date

let printNote (n: Note) =
    printHeader n.Header :: "" :: (n.Lines |> List.map printLine)
    |> String.concat "\n"
