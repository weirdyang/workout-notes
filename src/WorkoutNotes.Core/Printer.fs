module WorkoutNotes.Core.Printer

/// Decimal without trailing zeros: 72.50 → "72.5", 100.0 → "100".
let formatDecimal (d: decimal) =
    let s = string d

    if s.Contains "." then s.TrimEnd('0').TrimEnd('.') else s

let private amount (v: decimal) (unit: string) =
    formatDecimal v + (if unit = Parser.defaultUnit then "" else unit)

/// Weight as it appears in a row, without the separator. The default unit (kg) is left out.
let printWeight =
    function
    | Load(v, u) -> amount v u
    | Added(v, u) -> "+" + amount v u
    | Assisted(v, u) -> "-" + amount v u
    | Bodyweight -> "bw"
    | Unweighted -> "-"

let printRep (r: Rep) =
    match r.Mark with
    | Some m -> $"{r.Count}{m}"
    | None -> string r.Count

let printReps (reps: Rep list) =
    reps |> List.map printRep |> String.concat ","

/// Canonical row: `incline @ 85x3,3,3`, `dip @ +16x8,8`, `leg raise @ bw 20,20`, `plank @ - 60,60`.
let printRow (r: ExerciseRow) =
    let sets =
        match r.Weight with
        | Bodyweight
        | Unweighted -> $"{printWeight r.Weight} {printReps r.Reps}"
        | w -> $"{printWeight w}x{printReps r.Reps}"

    let note =
        match r.Note with
        | Some n -> $" // {n}"
        | None -> ""

    $"{r.Exercise} @ {sets}{note}"

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

/// Tidy one line: rows (including old `[exercise]` lines) become canonical; anything else is left exactly as typed.
let tidyLine (line: string) =
    match Parser.parseLine line with
    | Exercise r -> printRow r
    | _ -> line
