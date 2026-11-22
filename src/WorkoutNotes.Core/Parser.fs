module WorkoutNotes.Core.Parser

open System
open XParsec
open XParsec.Parsers
open XParsec.CharParsers
open XParsec.Combinators

let defaultUnit = "kg"

let private maxWeightLength = 9
let private maxRepDigits = 6

let private charsThatCannotBeMarks =
    set [ ','; '.'; '['; ']'; '/'; '+'; '-'; '−'; '@'; 'x'; 'X'; '×' ]

let isMarkChar (c: char) =
    not (Char.IsDigit c) && not (Char.IsWhiteSpace c) && not (charsThatCannotBeMarks.Contains c)

let normalizeUnit (unit: string) =
    match unit.Trim().ToLower() with
    | "" -> defaultUnit
    | "kgs" -> "kg"
    | "lbs" -> "lb"
    | unit -> unit

let private isBlank c = c = ' ' || c = '\t'
let private isTimesSign c = c = 'x' || c = 'X' || c = '×'
let private isDigitOrBlank c = Char.IsDigit c || isBlank c

let private blanks = skipMany1Satisfies isBlank
let private optionalBlanks = skipManySatisfies isBlank
let private digits = many1Chars (satisfy Char.IsDigit)
let private minusSign = pchar '-' <|> pchar '−'
let private repsFollow = lookAhead (satisfy Char.IsDigit)

let private weightRepsSeparator =
    (optionalBlanks >>. satisfy isTimesSign >>. optionalBlanks) <|> blanks

let private number =
    digits .>>. opt (pchar '.' >>. digits)
    >>= fun struct (whole, fraction) ->
        let text =
            match fraction with
            | ValueSome f -> whole + "." + f
            | ValueNone -> whole

        if text.Length > maxWeightLength then fail (Message "weight too large") else preturn (decimal text)

let private amountWithUnitThenSeparator =
    number .>>. manyChars (satisfy Char.IsLetter)
    >>= fun struct (value, letters) ->
        let lastLetterIsTimesSign = letters.Length > 0 && isTimesSign letters.[letters.Length - 1]

        let timesSignTypedRightAfterUnit =
            if lastLetterIsTimesSign then
                lookAhead (satisfy isDigitOrBlank)
                >>. optionalBlanks
                >>% (value, normalizeUnit (letters.Substring(0, letters.Length - 1)))
            else
                pzero

        let unitThenSeparator = weightRepsSeparator >>% (value, normalizeUnit letters)

        timesSignTypedRightAfterUnit <|> unitThenSeparator

let private bodyweightKeyword = pstring "bw" <|> pstring "BW" <|> pstring "Bw"

let private weight =
    choice
        [ bodyweightKeyword >>. weightRepsSeparator >>% Bodyweight
          pchar '+' >>. amountWithUnitThenSeparator |>> Added
          minusSign >>. amountWithUnitThenSeparator |>> Assisted
          minusSign >>. weightRepsSeparator >>% Unweighted
          amountWithUnitThenSeparator |>> Load ]
    .>> repsFollow

let private orUnweighted =
    function
    | ValueSome weight -> weight
    | ValueNone -> Unweighted

let private rep =
    digits .>>. opt (satisfy isMarkChar)
    >>= fun struct (count, mark) ->
        if count.Length > maxRepDigits then
            fail (Message "rep count too large")
        else
            preturn { Count = int count; Mark = ValueOption.toOption mark }

let private repsSeparatedByCommas =
    sepBy1 rep (optionalBlanks >>. pchar ',' .>> optionalBlanks)
    |>> fun struct (reps, _) -> List.ofSeq reps

let private restOfLine = manyChars (satisfy (fun _ -> true))

let private trailingNote =
    opt (optionalBlanks >>. pstring "//" >>. restOfLine)
    |>> fun note ->
        match note with
        | ValueSome text when text.Trim() <> "" -> Some(text.Trim())
        | _ -> None

let private weightRepsAndNote =
    optionalBlanks >>. (opt weight |>> orUnweighted)
    .>>. repsSeparatedByCommas
    .>>. trailingNote
    .>> optionalBlanks
    .>> eof

let private parseAfterAtSign (exercise: string) (afterAtSign: string) =
    match weightRepsAndNote (Reader.ofString afterAtSign ()) with
    | Ok parsed ->
        let struct (struct (weight, reps), note) = parsed

        Ok
            { Exercise = exercise
              Weight = weight
              Reps = reps
              Note = note }
    | Error error -> Error(ErrorFormatting.formatStringError afterAtSign error)

let private bracketRowWeight =
    choice
        [ bodyweightKeyword >>% Bodyweight
          pchar '+' >>. number |>> fun value -> Added(value, defaultUnit)
          minusSign >>. number |>> fun value -> Assisted(value, defaultUnit)
          number |>> fun value -> Load(value, defaultUnit) ]

let private bracketRow =
    pipe4
        (between (pchar '[') (pchar ']') (manyChars (satisfy ((<>) ']'))) .>> blanks)
        (opt (bracketRowWeight .>> blanks .>> repsFollow) |>> orUnweighted)
        repsSeparatedByCommas
        trailingNote
        (fun exercise weight reps note ->
            { Exercise = exercise.Trim()
              Weight = weight
              Reps = reps
              Note = note })
    .>> optionalBlanks
    .>> eof

let private parseRowWithAtSign (line: string) (trimmed: string) =
    let atSign = trimmed.IndexOf '@'
    let exercise = trimmed.Substring(0, atSign).Trim()

    if exercise = "" then
        Unparsed(line, "Exercise name missing before @")
    else
        match parseAfterAtSign exercise (trimmed.Substring(atSign + 1)) with
        | Ok row -> Exercise row
        | Error message -> Unparsed(line, message)

let private parseBracketRowOrText (trimmed: string) =
    match bracketRow (Reader.ofString trimmed ()) with
    | Ok row when row.Exercise <> "" -> Exercise row
    | _ -> Line.Text trimmed

let parseLine (line: string) : Line =
    let trimmed = line.Trim()

    if trimmed = "" then Blank
    elif trimmed.Contains "@" then parseRowWithAtSign line trimmed
    elif trimmed.StartsWith "[" then parseBracketRowOrText trimmed
    else Line.Text trimmed

let nameOnLine (line: string) =
    let trimmed = line.Trim()

    match trimmed.IndexOf '@' with
    | -1 -> trimmed
    | atSign -> trimmed.Substring(0, atSign).Trim()

let tryParseIsoDate (text: string) : DateOnly option =
    let allDigits (part: string) = part.Length > 0 && part |> Seq.forall Char.IsDigit

    if text.Length = 10 && text.[4] = '-' && text.[7] = '-' then
        let year, month, day = text.Substring(0, 4), text.Substring(5, 2), text.Substring(8, 2)

        if allDigits year && allDigits month && allDigits day then
            let year, month, day = int year, int month, int day

            if month >= 1 && month <= 12 && day >= 1 && day <= DateTime.DaysInMonth(year, month) then
                Some(DateOnly(year, month, day))
            else
                None
        else
            None
    else
        None

let private weekdayAbbreviations = set [ "sun"; "mon"; "tue"; "wed"; "thu"; "fri"; "sat" ]

let private withoutLeadingWeekday (text: string) =
    let words = text.Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries)

    if words.Length > 0 && weekdayAbbreviations.Contains(words.[0].ToLower()) then
        String.Join(" ", words.[1..])
    else
        text

let private titleAfterDate (afterDate: string) =
    let title =
        match afterDate.IndexOf '·' with
        | -1 -> withoutLeadingWeekday afterDate
        | middleDot -> afterDate.Substring(middleDot + 1)

    match title.Trim() with
    | "" -> None
    | title -> Some title

let parseHeader (line: string) : Result<Header, string> =
    let trimmed = line.Trim()
    let isoDateLength = 10

    if trimmed.Length < isoDateLength then
        Result.Error "header must start with a date (yyyy-MM-dd)"
    else
        let dateText = trimmed.Substring(0, isoDateLength)

        match tryParseIsoDate dateText with
        | None -> Result.Error $"invalid date: {dateText}"
        | Some date ->
            Result.Ok
                { Date = date
                  Title = titleAfterDate (trimmed.Substring isoDateLength) }

let private isBlankLine (line: string) = line.Trim() = ""

let parseNote (text: string) : Result<Note, string> =
    let lines = text.Replace("\r\n", "\n").Split('\n') |> List.ofArray

    match lines |> List.skipWhile isBlankLine with
    | [] -> Result.Error "note is empty"
    | header :: body ->
        let skipBlankLinesUnderHeader = List.skipWhile ((=) Blank)

        parseHeader header
        |> Result.map (fun header ->
            { Header = header
              Lines = body |> List.map parseLine |> skipBlankLinesUnderHeader })
