module WorkoutNotes.Core.Printer

/// Decimal without trailing zeros: 72.50 → "72.5", 100.0 → "100".
let formatDecimal (d: decimal) =
    let s = string d

    if s.Contains "." then s.TrimEnd('0').TrimEnd('.') else s

let private fmt (d: decimal<kg>) = formatDecimal (decimal d)

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
    let days = [| "Sun"; "Mon"; "Tue"; "Wed"; "Thu"; "Fri"; "Sat" |]
    let pad (n: int) w = (string n).PadLeft(w, '0')
    let d = h.Date
    let date = $"{pad d.Year 4}-{pad d.Month 2}-{pad d.Day 2} {days.[int d.DayOfWeek]}"

    match h.Title with
    | Some t -> $"{date} · {t}"
    | None -> date

let printNote (n: Note) =
    printHeader n.Header :: "" :: (n.Lines |> List.map printLine)
    |> String.concat "\n"
