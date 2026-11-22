module WorkoutNotes.Core.Parser

open System
open System.Globalization
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

        match Decimal.TryParse(text, NumberStyles.Number, CultureInfo.InvariantCulture) with
        | true, d -> preturn (d * 1.0m<kg>)
        | _ -> fail (Message "invalid weight")

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
        match Int32.TryParse(n, NumberStyles.None, CultureInfo.InvariantCulture) with
        | true, count -> preturn { Count = count; Mark = ValueOption.toOption mark }
        | _ -> fail (Message "rep count too large")

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

/// Header line: `2026-11-21 Sat · Incline + legs`. Date is required; title follows `·`.
let parseHeader (line: string) : Result<Header, string> =
    let t = line.Trim()

    if t.Length < 10 then
        Result.Error "header must start with a date (yyyy-MM-dd)"
    else
        match DateOnly.TryParseExact(t.Substring(0, 10), "yyyy-MM-dd", CultureInfo.InvariantCulture, DateTimeStyles.None) with
        | false, _ -> Result.Error $"invalid date: {t.Substring(0, 10)}"
        | true, date ->
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
