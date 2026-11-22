module WorkoutNotes.Core.Parser

open System
open XParsec
open XParsec.Parsers
open XParsec.CharParsers
open XParsec.Combinators

// XParsec (pure F#, compiles under Fable). Alternatives always backtrack, so no `attempt`.
// Grammar (spec §Row grammar):
//   row     = "[" name "]" ws [ weight ws ] reps [ ws "//" note ] ;
//   weight  = decimal | ("+" | "-") decimal | "BW" ;
//   reps    = rep { "," rep } ;
//   rep     = digit { digit } [ mark ] ;

/// Characters that can never be a rep mark.
let reservedChars = set [ ','; '.'; '['; ']'; '/'; '+'; '-'; '−' ]

let isMarkChar (c: char) =
    not (Char.IsDigit c) && not (Char.IsWhiteSpace c) && not (reservedChars.Contains c)

let private isBlank c = c = ' ' || c = '\t'
let private ws1 = skipMany1Satisfies isBlank
let private ws0 = skipManySatisfies isBlank
let private digits = many1Chars (satisfy Char.IsDigit)

let private name =
    between (pchar '[') (pchar ']') (manyChars (satisfy ((<>) ']')))
    >>= fun s ->
        let t = s.Trim()
        if t = "" then fail (Message "exercise name is empty") else preturn t

let private decimalKg =
    digits .>>. opt (pchar '.' >>. digits)
    >>= fun struct (whole, frac) ->
        let text =
            match frac with
            | ValueSome f -> whole + "." + f
            | ValueNone -> whole

        if text.Length > 9 then fail (Message "weight too large")
        else preturn (decimal text * 1.0m<kg>)

let private weight =
    choice
        [ pstring "BW" >>% Bodyweight
          pstring "bw" >>% Bodyweight
          pchar '+' >>. decimalKg |>> Added
          (pchar '-' <|> pchar '−') >>. decimalKg |>> Assisted
          decimalKg |>> Load ]

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

let private weightThenSpace =
    opt (weight .>> ws1 .>> lookAhead (satisfy Char.IsDigit))
    |>> fun v ->
        match v with
        | ValueSome w -> w
        | ValueNone -> Unweighted

let private row =
    pipe4 (name .>> ws1) weightThenSpace reps note (fun n w r nt ->
        { Exercise = n
          Weight = w
          Reps = r
          Note = nt })
    .>> ws0
    .>> eof

/// Parse one line. Never throws: anything that isn't a valid row is kept as Text or Unparsed.
let parseLine (line: string) : Line =
    let trimmed = line.Trim()

    if trimmed = "" then
        Blank
    elif trimmed.StartsWith "[" then
        match row (Reader.ofString trimmed ()) with
        | Ok success -> Exercise success
        | Error err -> Unparsed(line, ErrorFormatting.formatStringError trimmed err)
    else
        Line.Text trimmed

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

/// Header line: `2026-11-21 Sat · Incline + legs`. Date is required; title follows `·`.
let parseHeader (line: string) : Result<Header, string> =
    let t = line.Trim()

    if t.Length < 10 then
        Result.Error "header must start with a date (yyyy-MM-dd)"
    else
        match tryParseIsoDate (t.Substring(0, 10)) with
        | None -> Result.Error $"invalid date: {t.Substring(0, 10)}"
        | Some date ->
            let title =
                match t.IndexOf '·' with
                | -1 -> None
                | i ->
                    let s = t.Substring(i + 1).Trim()
                    if s = "" then None else Some s

            Result.Ok { Date = date; Title = title }

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

/// Bracket-free entry: `Low bar Smith 105 10` → `[Low bar Smith] 105 10`.
/// Picks the shortest leading name (must contain a letter) after which the rest parses as a full row.
/// Lines that already start with `[`, blanks and free text that doesn't end in a row come back unchanged.
let normalizeLine (line: string) : string =
    let t = line.Trim()

    if t = "" || t.StartsWith "[" || t.StartsWith "//" then
        line
    else
        let words = t.Split([| ' '; '\t' |], StringSplitOptions.RemoveEmptyEntries)

        seq { 1 .. words.Length - 1 }
        |> Seq.tryPick (fun k ->
            let name = String.Join(" ", words.[.. k - 1])
            let rest = String.Join(" ", words.[k..])
            let candidate = $"[{name}] {rest}"

            if name |> Seq.exists Char.IsLetter then
                match parseLine candidate with
                | Exercise _ -> Some candidate
                | _ -> None
            else
                None)
        |> Option.defaultValue line
