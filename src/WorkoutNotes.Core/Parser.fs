module WorkoutNotes.Core.Parser

open System
open XParsec
open XParsec.Parsers
open XParsec.CharParsers
open XParsec.Combinators

// XParsec (pure F#, compiles under Fable). Alternatives always backtrack, so no `attempt`.
//
// Row grammar:
//   row     = name "@" weight? reps note?          spaces optional around every part
//   weight  = ["+" | "-"] number [unit] sep | "bw" sep | "-" sep
//   sep     = "x" | "×" | spaces                    "85x3", "85 x 3", "85 3"
//   unit    = letters                               "kg" (default), "lb", "plates"; "85kgx3" = 85 kg for 3
//   reps    = rep { "," rep }
//   rep     = digits [mark]
//   note    = "//" text
// A line is a row only if it contains "@". Older "[exercise] weight reps" lines are still read.

/// The unit a number gets when none is written.
let defaultUnit = "kg"

/// Characters that can never be a rep mark.
let reservedChars = set [ ','; '.'; '['; ']'; '/'; '+'; '-'; '−'; '@'; 'x'; 'X'; '×' ]

let isMarkChar (c: char) =
    not (Char.IsDigit c) && not (Char.IsWhiteSpace c) && not (reservedChars.Contains c)

let normalizeUnit (u: string) =
    match u.Trim().ToLower() with
    | "" -> defaultUnit
    | "kgs" -> "kg"
    | "lbs" -> "lb"
    | u -> u

let private isBlank c = c = ' ' || c = '\t'
let private ws1 = skipMany1Satisfies isBlank
let private ws0 = skipManySatisfies isBlank
let private digits = many1Chars (satisfy Char.IsDigit)
let private isX c = c = 'x' || c = 'X' || c = '×'

/// Between weight and reps: "x" (spaces optional) or plain spaces.
let private sep = (ws0 >>. satisfy isX >>. ws0) <|> ws1

let private number =
    digits .>>. opt (pchar '.' >>. digits)
    >>= fun struct (whole, frac) ->
        let text =
            match frac with
            | ValueSome f -> whole + "." + f
            | ValueNone -> whole

        if text.Length > 9 then fail (Message "weight too large") else preturn (decimal text)

/// A number, its unit and the separator. "85kgx3": the unit's trailing x before a digit is the separator.
let private amount =
    number .>>. manyChars (satisfy Char.IsLetter)
    >>= fun struct (value, letters) ->
        let endsInX = letters.Length > 0 && isX letters.[letters.Length - 1]

        let withX =
            if endsInX then
                lookAhead (satisfy (fun c -> Char.IsDigit c || isBlank c))
                >>. ws0
                >>% (value, normalizeUnit (letters.Substring(0, letters.Length - 1)))
            else
                pzero

        withX <|> (sep >>% (value, normalizeUnit letters))

let private weight =
    choice
        [ pstring "bw" >>. sep >>% Bodyweight
          pstring "BW" >>. sep >>% Bodyweight
          pstring "Bw" >>. sep >>% Bodyweight
          pchar '+' >>. amount |>> Added
          (pchar '-' <|> pchar '−') >>. amount |>> Assisted
          (pchar '-' <|> pchar '−') >>. sep >>% Unweighted
          amount |>> Load ]
    .>> lookAhead (satisfy Char.IsDigit)

let private rep =
    digits .>>. opt (satisfy isMarkChar)
    >>= fun struct (n, mark) ->
        if n.Length > 6 then fail (Message "rep count too large")
        else preturn { Count = int n; Mark = ValueOption.toOption mark }

let private reps =
    sepBy1 rep (ws0 >>. pchar ',' .>> ws0) |>> fun struct (items, _) -> List.ofSeq items

let private note =
    opt (ws0 >>. pstring "//" >>. manyChars (satisfy (fun _ -> true)))
    |>> fun v ->
        match v with
        | ValueSome s when s.Trim() <> "" -> Some(s.Trim())
        | _ -> None

/// Everything after the "@".
let private afterAt =
    ws0 >>. (opt weight |>> fun w -> match w with ValueSome w -> w | ValueNone -> Unweighted)
    .>>. reps
    .>>. note
    .>> ws0
    .>> eof

let private runAfterAt (name: string) (rest: string) =
    match afterAt (Reader.ofString rest ()) with
    | Ok parsed ->
        let struct (struct (w, r), nt) = parsed
        Ok { Exercise = name; Weight = w; Reps = r; Note = nt }
    | Error err -> Error(ErrorFormatting.formatStringError rest err)

// ---------------------------------------------------------------- legacy "[exercise] weight reps"

let private legacyWeight =
    choice
        [ pstring "BW" >>% Bodyweight
          pstring "bw" >>% Bodyweight
          pchar '+' >>. number |>> fun v -> Added(v, defaultUnit)
          (pchar '-' <|> pchar '−') >>. number |>> fun v -> Assisted(v, defaultUnit)
          number |>> fun v -> Load(v, defaultUnit) ]

let private legacyRow =
    pipe4
        (between (pchar '[') (pchar ']') (manyChars (satisfy ((<>) ']'))) .>> ws1)
        (opt (legacyWeight .>> ws1 .>> lookAhead (satisfy Char.IsDigit))
         |>> fun v -> match v with ValueSome w -> w | ValueNone -> Unweighted)
        reps
        note
        (fun n w r nt -> { Exercise = n.Trim(); Weight = w; Reps = r; Note = nt })
    .>> ws0
    .>> eof

// ---------------------------------------------------------------- lines

/// Parse one line. Never throws: anything that isn't a valid row is kept as Text or Unparsed.
let parseLine (line: string) : Line =
    let trimmed = line.Trim()

    if trimmed = "" then
        Blank
    elif trimmed.Contains "@" then
        let at = trimmed.IndexOf '@'
        let name = trimmed.Substring(0, at).Trim()

        if name = "" then
            Unparsed(line, "Exercise name missing before @")
        else
            match runAfterAt name (trimmed.Substring(at + 1)) with
            | Ok row -> Exercise row
            | Error msg -> Unparsed(line, msg)
    elif trimmed.StartsWith "[" then
        match legacyRow (Reader.ofString trimmed ()) with
        | Ok row when row.Exercise <> "" -> Exercise row
        | _ -> Line.Text trimmed
    else
        Line.Text trimmed

/// The exercise name typed so far on a line: the text before "@", or the whole line when there is no "@".
let nameOnLine (line: string) =
    let t = line.Trim()
    let i = t.IndexOf '@'
    (if i >= 0 then t.Substring(0, i) else t).Trim()

// ---------------------------------------------------------------- header and note

/// `yyyy-MM-dd` → DateOnly, without culture APIs (runs under Fable too).
let tryParseIsoDate (s: string) : DateOnly option =
    let isDigits (x: string) = x.Length > 0 && x |> Seq.forall Char.IsDigit

    if s.Length = 10 && s.[4] = '-' && s.[7] = '-' then
        let y, m, d = s.Substring(0, 4), s.Substring(5, 2), s.Substring(8, 2)

        if isDigits y && isDigits m && isDigits d then
            let y, m, d = int y, int m, int d

            if m >= 1 && m <= 12 && d >= 1 && d <= DateTime.DaysInMonth(y, m) then
                Some(DateOnly(y, m, d))
            else
                None
        else
            None
    else
        None

let private weekdays = set [ "sun"; "mon"; "tue"; "wed"; "thu"; "fri"; "sat" ]

/// Header line: `2026-11-21 Sat · Incline + legs`, `2026-11-21 push` or `2026-11-21`.
/// Date is required; the title is whatever follows it (after `·` if there is one, else after an optional weekday).
let parseHeader (line: string) : Result<Header, string> =
    let t = line.Trim()

    if t.Length < 10 then
        Result.Error "header must start with a date (yyyy-MM-dd)"
    else
        match tryParseIsoDate (t.Substring(0, 10)) with
        | None -> Result.Error $"invalid date: {t.Substring(0, 10)}"
        | Some date ->
            let rest = t.Substring(10).Trim()

            let title =
                match rest.IndexOf '·' with
                | -1 ->
                    let words = rest.Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries)

                    if words.Length > 0 && weekdays.Contains(words.[0].ToLower()) then
                        String.Join(" ", words.[1..])
                    else
                        rest
                | i -> rest.Substring(i + 1).Trim()

            Result.Ok
                { Date = date
                  Title = if title = "" then None else Some title }

/// Parse a whole note: first non-blank line is the header, every other line is a Line.
let parseNote (text: string) : Result<Note, string> =
    let lines = text.Replace("\r\n", "\n").Split('\n') |> List.ofArray
    let rest = lines |> List.skipWhile (fun l -> l.Trim() = "")

    match rest with
    | [] -> Result.Error "note is empty"
    | header :: body ->
        parseHeader header
        |> Result.map (fun h ->
            { Header = h
              Lines =
                body
                |> List.map parseLine
                // drop the blank line(s) directly under the header
                |> List.skipWhile ((=) Blank) })
