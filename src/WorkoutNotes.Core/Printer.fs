module WorkoutNotes.Core.Printer

let formatDecimal (value: decimal) =
    let text = string value

    if text.Contains "." then text.TrimEnd('0').TrimEnd('.') else text

let private unitSuffix (unit: string) =
    if unit = Parser.defaultUnit then "" else unit

let private amountWithUnit (value: decimal) (unit: string) = formatDecimal value + unitSuffix unit

let printWeight =
    function
    | Load(value, unit) -> amountWithUnit value unit
    | Added(value, unit) -> "+" + amountWithUnit value unit
    | Assisted(value, unit) -> "-" + amountWithUnit value unit
    | Bodyweight -> "bw"
    | Unweighted -> "-"

let printRep (rep: Rep) =
    match rep.Mark with
    | Some mark -> $"{rep.Count}{mark}"
    | None -> string rep.Count

let printReps (reps: Rep list) =
    reps |> List.map printRep |> String.concat ","

let printWeightAndReps (row: ExerciseRow) =
    match row.Weight with
    | Bodyweight
    | Unweighted -> $"{printWeight row.Weight} {printReps row.Reps}"
    | weight -> $"{printWeight weight}x{printReps row.Reps}"

let printRow (row: ExerciseRow) =
    let note =
        match row.Note with
        | Some note -> $" // {note}"
        | None -> ""

    $"{row.Exercise} @ {printWeightAndReps row}{note}"

let printLine =
    function
    | Exercise row -> printRow row
    | Text text -> text
    | Blank -> ""
    | Unparsed(raw, _) -> raw

let printHeader (header: Header) =
    let weekdayAbbreviations = [| "Sun"; "Mon"; "Tue"; "Wed"; "Thu"; "Fri"; "Sat" |]
    let zeroPadded (width: int) (value: int) = (string value).PadLeft(width, '0')
    let date = header.Date

    let dateWithWeekday =
        $"{zeroPadded 4 date.Year}-{zeroPadded 2 date.Month}-{zeroPadded 2 date.Day} {weekdayAbbreviations.[int date.DayOfWeek]}"

    match header.Title with
    | Some title -> $"{dateWithWeekday} · {title}"
    | None -> dateWithWeekday

let printNote (note: Note) =
    printHeader note.Header :: "" :: (note.Lines |> List.map printLine)
    |> String.concat "\n"

let tidyLine (line: string) =
    match Parser.parseLine line with
    | Exercise row -> printRow row
    | _ -> line
