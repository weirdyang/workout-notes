module App

open System
open Fable.Core
open Elmish
open Feliz
open Browser
open WorkoutNotes.Core
open WorkoutNotes.Core.History
open Storage

// ---------------------------------------------------------------- model

type Page =
    | NotesList
    | Editor of noteId: string
    | ExerciseHistory of exercise: string * back: Page
    | LegendPage

type Model =
    { Notes: StoredNote list
      Parsed: Map<string, Note option>
      Rows: DatedRow list
      Badges: Map<string * int, Badge>
      Legend: MarkDef list
      Page: Page
      Search: string
      Caret: int
      NewMark: string
      Toast: string option }

type Msg =
    | OpenNote of string
    | NewNote
    | Back
    | TextChanged of string * int
    | CaretMoved of int
    | EditorBlurred
    | CopyNote
    | DeleteNote
    | OpenHistory of string
    | OpenLegend
    | SetSearch of string
    | ExportNotes
    | ImportNotes of string
    | SetMarkLabel of char * string
    | SetMarkBehaviour of char * Behaviour
    | DeleteMark of char
    | SetNewMark of string
    | AddMark
    | ShowToast of string
    | ClearToast

let legendMap (legend: MarkDef list) : Legend =
    legend |> List.map (fun m -> m.Symbol, m) |> Map.ofList

let today () =
    let n = DateTime.Now
    DateOnly(n.Year, n.Month, n.Day)

let private parseStored (n: StoredNote) =
    match Parser.parseNote n.Text with
    | Ok note -> Some note
    | Error _ -> None

/// Recompute everything derived from the notes. Only `changed` notes are re-parsed.
let private derive (changed: string list option) (model: Model) =
    let parsed =
        match changed with
        | None -> model.Notes |> List.map (fun n -> n.Id, parseStored n) |> Map.ofList
        | Some ids ->
            let keep = model.Notes |> List.map (fun n -> n.Id) |> Set.ofList
            let current = model.Parsed |> Map.filter (fun id _ -> keep.Contains id)

            model.Notes
            |> List.filter (fun n -> List.contains n.Id ids || not (current.ContainsKey n.Id))
            |> List.fold (fun acc n -> Map.add n.Id (parseStored n) acc) current

    let rows =
        parsed |> Map.toSeq |> Seq.choose (fun (id, n) -> n |> Option.map (fun n -> id, n)) |> rowsOf

    { model with
        Parsed = parsed
        Rows = rows
        Badges = allBadges (legendMap model.Legend) rows }

let init () =
    { Notes = loadNotes ()
      Parsed = Map.empty
      Rows = []
      Badges = Map.empty
      Legend = loadLegend ()
      Page = NotesList
      Search = ""
      Caret = 0
      NewMark = ""
      Toast = None }
    |> derive None,
    Cmd.none

// ---------------------------------------------------------------- helpers

let private setCaret (pos: int) : Cmd<Msg> =
    Cmd.ofEffect (fun _ ->
        JS.setTimeout
            (fun () ->
                match document.getElementById "editor" with
                | null -> ()
                | el ->
                    let ta = el :?> Types.HTMLTextAreaElement
                    ta.focus ()
                    ta.setSelectionRange (pos, pos))
            0
        |> ignore)

let private toastCmd (text: string) : Cmd<Msg> =
    Cmd.ofMsg (ShowToast text)

let private noteById (model: Model) id =
    model.Notes |> List.tryFind (fun n -> n.Id = id)

let private updateNoteText (id: string) (text: string) (model: Model) =
    let notes =
        model.Notes
        |> List.map (fun n ->
            if n.Id = id then
                { n with
                    Text = text
                    Updated = JS.Constructors.Date.now () }
            else
                n)

    saveNotes notes
    { model with Notes = notes } |> derive (Some [ id ])

let private headerFor (date: DateOnly) =
    Printer.printHeader { Date = date; Title = None }

let private dayNames = [| "Sun"; "Mon"; "Tue"; "Wed"; "Thu"; "Fri"; "Sat" |]
let private monthNames = [| "Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun"; "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec" |]

let formatDate (d: DateOnly) =
    let y = if d.Year = DateTime.Now.Year then "" else $" {d.Year}"
    $"{dayNames.[int d.DayOfWeek]} {d.Day} {monthNames.[d.Month - 1]}{y}"

let private monthLabel (d: DateOnly) =
    let full = [| "January"; "February"; "March"; "April"; "May"; "June"; "July"; "August"; "September"; "October"; "November"; "December" |]
    $"{full.[d.Month - 1]} {d.Year}"

let weightText =
    function
    | Load d -> Printer.formatDecimal (decimal d)
    | Added d -> "+" + Printer.formatDecimal (decimal d)
    | Assisted d -> "-" + Printer.formatDecimal (decimal d)
    | Bodyweight -> "BW"
    | Unweighted -> ""

let repsText (reps: Rep list) =
    reps |> List.map Printer.printRep |> String.concat ","

let setText (r: ExerciseRow) =
    match weightText r.Weight with
    | "" -> repsText r.Reps
    | w -> $"{w} × {repsText r.Reps}"

/// Lines of the note body as the parser sees them, with each one's line number in the text.
let private bodyLines (text: string) =
    let lines = text.Split('\n')
    let body = lines |> Array.skip (min 1 lines.Length)

    let skipped =
        body |> Array.takeWhile (fun l -> l.Trim() = "") |> Array.length

    body
    |> Array.skip skipped
    |> Array.mapi (fun i l -> i, 1 + skipped + i, l)

/// Exercise name on a line, whether bracketed or not yet.
let private exerciseOnLine (line: string) =
    match Parser.parseLine (Parser.normalizeLine line) with
    | Exercise r -> Some r.Exercise
    | _ ->
        let t = line.Trim()

        if t.StartsWith "[" && t.Contains "]" then
            let n = t.Substring(1, t.IndexOf ']' - 1).Trim()
            if n = "" then None else Some n
        else
            None

/// True when a line names an exercise but has no weight or reps yet.
let private onlyName (line: string) =
    let t = line.Trim()
    t.StartsWith "[" && t.EndsWith "]"

// ---------------------------------------------------------------- edits
// Edits are applied to the textarea directly (value + caret together), then reported with TextChanged.
// Doing it in one synchronous step keeps the caret where it belongs, even when typing fast.

let private enterEdit (text: string) (caret: int) =
    let lineNo = Editing.lineIndexAt text caret

    let text, caret =
        if lineNo > 0 then
            let line = Editing.currentLine text caret
            let s, _ = Editing.lineBounds text caret
            let normalized = Parser.normalizeLine line
            let t, _ = Editing.replaceLine text caret normalized
            let caretInLine = caret - s + (normalized.Length - line.Length)
            t, s + max 0 (min caretInLine normalized.Length)
        else
            text, caret

    Editing.insertAt text caret "\n"

let private repeatEdit (text: string) (caret: int) =
    let lineNo = Editing.lineIndexAt text caret
    let lines = text.Split('\n')

    lines
    |> Array.take (min lineNo lines.Length)
    |> Array.rev
    |> Array.tryPick (fun l ->
        match Parser.parseLine (Parser.normalizeLine l) with
        | Exercise r -> Some(Printer.printRow r)
        | _ -> None)
    |> Option.map (fun row ->
        if (Editing.currentLine text caret).Trim() = "" then
            Editing.replaceLine text caret row
        else
            let _, e = Editing.lineBounds text caret
            Editing.insertAt text e ("\n" + row))

let private editInPlace (dispatch: Msg -> unit) (edit: string -> int -> (string * int) option) =
    match document.getElementById "editor" with
    | null -> ()
    | el ->
        let ta = el :?> Types.HTMLTextAreaElement

        match edit ta.value (int ta.selectionStart) with
        | Some(text, caret) ->
            ta.value <- text
            ta.focus ()
            ta.setSelectionRange (caret, caret)
            dispatch (TextChanged(text, caret))
        | None -> ()

// ---------------------------------------------------------------- update

let private editing (model: Model) f =
    match model.Page with
    | Editor id ->
        match noteById model id with
        | Some note -> f id note.Text
        | None -> model, Cmd.none
    | _ -> model, Cmd.none

let private dropIfEmpty (model: Model) id =
    match noteById model id with
    | Some n when n.Text.Split('\n') |> Array.skip 1 |> Array.forall (fun l -> l.Trim() = "") ->
        let notes = model.Notes |> List.filter (fun x -> x.Id <> id)
        saveNotes notes
        { model with Notes = notes } |> derive (Some [])
    | _ -> model

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | OpenNote id -> { model with Page = Editor id }, Cmd.none

    | NewNote ->
        let id = newId ()
        let text = headerFor (today ()) + " · "

        let note =
            { Id = id
              Text = text
              Updated = JS.Constructors.Date.now () }

        let notes = note :: model.Notes
        saveNotes notes

        { model with
            Notes = notes
            Page = Editor id
            Caret = text.Length }
        |> derive (Some [ id ]),
        setCaret text.Length

    | Back ->
        match model.Page with
        | Editor id ->
            let m = editing model (fun id text -> updateNoteText id (Editing.normalizeAll Parser.normalizeLine text) model, Cmd.none) |> fst
            { dropIfEmpty m id with Page = NotesList }, Cmd.none
        | ExerciseHistory(_, back) -> { model with Page = back }, Cmd.none
        | LegendPage
        | NotesList -> { model with Page = NotesList }, Cmd.none

    | TextChanged(text, caret) ->
        editing model (fun id _ -> { updateNoteText id text model with Caret = caret }, Cmd.none)

    | CaretMoved caret -> { model with Caret = caret }, Cmd.none

    | EditorBlurred ->
        editing model (fun id text ->
            let normalized = Editing.normalizeAll Parser.normalizeLine text
            if normalized = text then model, Cmd.none else updateNoteText id normalized model, Cmd.none)

    | CopyNote ->
        editing model (fun _ text ->
            model,
            Cmd.batch
                [ Cmd.ofEffect (fun _ -> copyToClipboard (Editing.normalizeAll Parser.normalizeLine text) |> ignore)
                  toastCmd "Copied" ])

    | DeleteNote ->
        editing model (fun id _ ->
            if window.confirm "Delete this note?" then
                let notes = model.Notes |> List.filter (fun n -> n.Id <> id)
                saveNotes notes
                { model with Notes = notes; Page = NotesList } |> derive (Some []), Cmd.none
            else
                model, Cmd.none)

    | OpenHistory name ->
        let model =
            match model.Page with
            | Editor id -> { (editing model (fun id text -> updateNoteText id (Editing.normalizeAll Parser.normalizeLine text) model, Cmd.none) |> fst) with Page = model.Page }
            | _ -> model

        { model with Page = ExerciseHistory(name, model.Page) }, Cmd.none

    | OpenLegend -> { model with Page = LegendPage }, Cmd.none
    | SetSearch s -> { model with Search = s }, Cmd.none

    | ExportNotes ->
        let d = today ()
        let name = $"workout-notes-{d.Year}-{d.Month:D2}-{d.Day:D2}.md"
        let ordered = model.Notes |> List.sortBy (fun n -> n.Updated)
        download name (exportText ordered)
        model, toastCmd $"Exported {model.Notes.Length} notes"

    | ImportNotes text ->
        let existing = model.Notes |> List.map (fun n -> n.Text.Trim()) |> Set.ofList

        let added =
            splitImport text
            |> List.filter (fun t -> not (existing.Contains t))
            |> List.map (fun t ->
                { Id = newId ()
                  Text = t
                  Updated = JS.Constructors.Date.now () })

        let notes = added @ model.Notes
        saveNotes notes
        { model with Notes = notes } |> derive None, toastCmd $"Imported {added.Length} notes"

    | SetMarkLabel(sym, label) ->
        let legend = model.Legend |> List.map (fun m -> if m.Symbol = sym then { m with Label = label } else m)
        saveLegend legend
        { model with Legend = legend }, Cmd.none

    | SetMarkBehaviour(sym, b) ->
        let legend = model.Legend |> List.map (fun m -> if m.Symbol = sym then { m with Behaviour = b } else m)
        saveLegend legend
        { model with Legend = legend } |> derive (Some []), Cmd.none

    | DeleteMark sym ->
        let legend = model.Legend |> List.filter (fun m -> m.Symbol <> sym)
        saveLegend legend
        { model with Legend = legend } |> derive (Some []), Cmd.none

    | SetNewMark s -> { model with NewMark = s }, Cmd.none

    | AddMark ->
        let s = model.NewMark.Trim()

        if s.Length <> 1 then
            model, toastCmd "A mark is one character"
        elif not (Parser.isMarkChar s.[0]) then
            model, toastCmd $"'{s}' can't be a mark"
        elif model.Legend |> List.exists (fun m -> m.Symbol = s.[0]) then
            model, toastCmd $"'{s}' is already defined"
        else
            let legend = model.Legend @ [ { Symbol = s.[0]; Label = "New mark"; Behaviour = LabelOnly } ]
            saveLegend legend
            { model with Legend = legend; NewMark = "" } |> derive (Some []), Cmd.none

    | ShowToast t ->
        { model with Toast = Some t },
        Cmd.ofEffect (fun dispatch -> JS.setTimeout (fun () -> dispatch ClearToast) 1800 |> ignore)
    | ClearToast -> { model with Toast = None }, Cmd.none

// ---------------------------------------------------------------- view helpers

let private icon (svg: string) =
    Html.span [ prop.style [ style.display.inlineFlex ]; prop.ariaHidden true; prop.dangerouslySetInnerHTML svg ]

let private chevron =
    icon """<svg width="22" height="22" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2.2" stroke-linecap="round" stroke-linejoin="round"><path d="M15 18l-6-6 6-6"/></svg>"""

let private plus =
    icon """<svg width="26" height="26" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2.4" stroke-linecap="round"><path d="M12 5v14M5 12h14"/></svg>"""

let private gear =
    icon """<svg width="22" height="22" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><path d="M4 7h10M18 7h2M4 17h4M12 17h8"/><circle cx="16" cy="7" r="2"/><circle cx="10" cy="17" r="2"/></svg>"""

let private repeatIcon =
    icon """<svg width="20" height="20" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2.2" stroke-linecap="round" stroke-linejoin="round"><path d="M17 2l4 4-4 4"/><path d="M3 11v-1a4 4 0 0 1 4-4h14"/><path d="M7 22l-4-4 4-4"/><path d="M21 13v1a4 4 0 0 1-4 4H3"/></svg>"""

let private trash =
    icon """<svg width="20" height="20" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><path d="M3 6h18M8 6V4h8v2M6 6l1 14h10l1-14"/></svg>"""

let private backButton (label: string) dispatch =
    Html.button [ prop.className "link"; prop.onClick (fun _ -> dispatch Back); prop.children [ chevron; Html.text label ] ]

/// A button that doesn't steal focus from the editor.
let private keepFocus = prop.onMouseDown (fun e -> e.preventDefault ())

let private badgeView (b: Badge option) =
    match b with
    | Some LoadHigh -> Html.span [ prop.className "badge"; prop.text "L+"; prop.title "Load high" ]
    | Some RepHigh -> Html.span [ prop.className "badge"; prop.text "R+"; prop.title "Rep high" ]
    | Some Tie -> Html.span [ prop.className "badge tie"; prop.text "="; prop.title "Ties your best" ]
    | None -> Html.span []

let private chip (name: string) dispatch =
    Html.button [
        prop.className "chip"
        keepFocus
        prop.onClick (fun _ -> dispatch (OpenHistory name))
        prop.children [ Html.span [ prop.className "br"; prop.text "[" ]; Html.text name; Html.span [ prop.className "br"; prop.text "]" ] ]
    ]

// ---------------------------------------------------------------- notes list

let private noteDate (model: Model) (n: StoredNote) =
    match Map.tryFind n.Id model.Parsed with
    | Some(Some note) -> Some note.Header.Date
    | _ -> None

let private notesListView (model: Model) dispatch =
    let q = model.Search.Trim().ToLower()

    let notes =
        model.Notes
        |> List.filter (fun n -> q = "" || n.Text.ToLower().Contains q)
        |> List.sortByDescending (fun n ->
            (noteDate model n |> Option.defaultValue DateOnly.MinValue), n.Updated)

    let item (n: StoredNote) =
        let parsed = Map.tryFind n.Id model.Parsed |> Option.flatten

        let title, date, rows =
            match parsed with
            | Some note ->
                let rows = note.Lines |> List.choose (function Exercise r -> Some r | _ -> None)
                note.Header.Title |> Option.defaultValue "Untitled", formatDate note.Header.Date, rows
            | None -> n.Text.Split('\n').[0], "No date", []

        let preview =
            match rows with
            | [] -> "Empty"
            | first :: _ ->
                let count = rows |> List.distinctBy (fun r -> exerciseKey r.Exercise) |> List.length
                let s = if count = 1 then "" else "s"
                $"{Printer.printRow first} · {count} exercise{s}"

        let highs =
            model.Badges
            |> Map.filter (fun (id, _) b -> id = n.Id && b <> Tie)
            |> Map.count

        Html.button [
            prop.key n.Id
            prop.className "note-item"
            prop.onClick (fun _ -> dispatch (OpenNote n.Id))
            prop.children [
                Html.div [ prop.className "row1"; prop.children [ Html.span [ prop.className "name"; prop.text title ]; Html.span [ prop.className "date"; prop.text date ] ] ]
                Html.div [
                    prop.className "row2"
                    prop.children [
                        Html.span [ prop.className "preview"; prop.text preview ]
                        if highs > 0 then
                            Html.span [ prop.className "badge"; prop.text (if highs = 1 then "1 high" else $"{highs} highs") ]
                    ]
                ]
            ]
        ]

    let groups =
        notes
        |> List.groupBy (fun n -> noteDate model n |> Option.map monthLabel |> Option.defaultValue "No date")

    Html.div [
        prop.className "screen"
        prop.children [
            Html.div [
                prop.className "topbar"
                prop.children [
                    Html.span []
                    Html.button [ prop.className "icon-btn"; prop.ariaLabel "Legend and settings"; prop.onClick (fun _ -> dispatch OpenLegend); prop.children [ gear ] ]
                ]
            ]
            Html.h1 [ prop.className "title"; prop.text "Notes" ]
            Html.label [
                prop.className "search"
                prop.children [
                    Html.span [ prop.className "sr-only"; prop.text "Search notes" ]
                    Html.input [
                        prop.type' "search"
                        prop.placeholder "Search exercises, marks, notes"
                        prop.value model.Search
                        prop.onChange (fun (s: string) -> dispatch (SetSearch s))
                    ]
                ]
            ]
            Html.div [
                prop.className "scroll"
                prop.children [
                    if model.Notes.IsEmpty then
                        Html.p [ prop.className "empty"; prop.text "No notes yet. Tap + to log today's session — one line per exercise, like: Incline 85 6,6,6" ]
                    else
                        for label, ns in groups do
                            Html.div [ prop.key label; prop.className "section-label"; prop.text label ]
                            for n in ns do
                                item n
                    Html.div [ prop.style [ style.height 96 ] ]
                ]
            ]
            Html.button [ prop.className "fab"; prop.ariaLabel "New note for today"; prop.onClick (fun _ -> dispatch NewNote); prop.children [ plus ] ]
        ]
    ]

// ---------------------------------------------------------------- editor

let private editorView (model: Model) (note: StoredNote) dispatch =
    let text = note.Text
    let caretLine = Editing.lineIndexAt text model.Caret
    let line = Editing.currentLine text model.Caret
    let before = Editing.beforeCaret text model.Caret
    let parsedNote = Map.tryFind note.Id model.Parsed |> Option.flatten
    let noteDate = parsedNote |> Option.map (fun n -> n.Header.Date) |> Option.defaultValue (today ())

    // Suggestions: on a body line that is still just a name (no digits yet).
    let suggestions =
        let t = before.Trim().TrimStart('[')

        if caretLine = 0 || before.Contains "]" || before.Trim().StartsWith "//" || before |> Seq.exists Char.IsDigit then
            []
        else
            let q = exerciseKey t

            exerciseNames model.Rows
            |> List.filter (fun n -> q = "" || (exerciseKey n).Contains q)
            |> List.filter (fun n -> exerciseKey n <> q || line.Trim() = "")
            |> List.truncate 8

    // Last time for the exercise on this line, and the matching set to fill in.
    let lastHint =
        if caretLine = 0 then
            None
        else
            exerciseOnLine line
            |> Option.bind (fun name ->
                lastSession model.Rows name noteDate note.Id
                |> Option.map (fun (date, rows) ->
                    let above =
                        text.Split('\n')
                        |> Array.take caretLine
                        |> Array.filter (fun l -> exerciseOnLine l |> Option.map exerciseKey = Some(exerciseKey name))
                        |> Array.length

                    let pick = rows.[min above (rows.Length - 1)]
                    name, date, rows, pick))

    let body = bodyLines text

    Html.div [
        prop.className "screen"
        prop.children [
            Html.div [
                prop.className "topbar"
                prop.children [
                    backButton "Notes" dispatch
                    Html.div [
                        prop.style [ style.display.flex; style.alignItems.center ]
                        prop.children [
                            Html.button [ prop.className "icon-btn"; prop.ariaLabel "Delete note"; prop.onClick (fun _ -> dispatch DeleteNote); prop.children [ trash ] ]
                            Html.button [ prop.className "link strong"; prop.onClick (fun _ -> dispatch CopyNote); prop.text "Copy" ]
                        ]
                    ]
                ]
            ]
            if parsedNote.IsNone then
                Html.div [ prop.className "perr"; prop.text "First line must start with a date, like 2026-11-22 · Push" ]
            Html.div [
                prop.className "editor-wrap"
                prop.children [
                    Html.textarea [
                        prop.id "editor"
                        prop.className "editor"
                        prop.ariaLabel "Note"
                        prop.custom ("spellCheck", false)
                        prop.custom ("autoCapitalize", "off")
                        prop.autoComplete "off"
                        prop.custom ("autoCorrect", "off")
                        prop.value text
                        prop.onChange (fun (e: Types.Event) ->
                            let ta = e.target :?> Types.HTMLTextAreaElement
                            dispatch (TextChanged(ta.value, int ta.selectionStart)))
                        prop.onSelect (fun (e: Types.Event) ->
                            let ta = e.target :?> Types.HTMLTextAreaElement
                            dispatch (CaretMoved(int ta.selectionStart)))
                        prop.onClick (fun e ->
                            let ta = e.target :?> Types.HTMLTextAreaElement
                            dispatch (CaretMoved(int ta.selectionStart)))
                        prop.onKeyDown (fun (e: Types.KeyboardEvent) ->
                            if e.key = "Enter" && not e.shiftKey then
                                e.preventDefault ()
                                editInPlace dispatch (fun t c -> Some(enterEdit t c)))
                        prop.onBlur (fun _ -> dispatch EditorBlurred)
                    ]
                    Html.div [
                        prop.className "parsed"
                        prop.children [
                            for (i, lineNo, raw) in body do
                                match Parser.parseLine raw with
                                | Exercise r ->
                                    let b = if parsedNote.IsSome then Map.tryFind (note.Id, i) model.Badges else None

                                    Html.div [
                                        prop.key lineNo
                                        prop.className (if lineNo = caretLine then "prow current" else "prow")
                                        prop.children [
                                            chip r.Exercise dispatch
                                            Html.span [ prop.className "w"; prop.text (weightText r.Weight) ]
                                            Html.span [ prop.text (repsText r.Reps) ]
                                            Html.span [ prop.className "b"; prop.children [ badgeView b ] ]
                                        ]
                                    ]
                                | Unparsed(rawLine, _) when lineNo <> caretLine ->
                                    Html.div [
                                        prop.key lineNo
                                        prop.className "perr"
                                        prop.children [ Html.text "Not a row: "; Html.code (rawLine.Trim()) ]
                                    ]
                                | _ -> ()
                        ]
                    ]
                ]
            ]
            Html.div [
                prop.className "keybar"
                prop.children [
                    match lastHint with
                    | Some(_, date, rows, pick) ->
                        Html.div [
                            prop.className "hint"
                            prop.children [
                                Html.span [ prop.text $"""Last · {formatDate date} · {rows |> List.map setText |> String.concat " · "}""" ]
                                if onlyName line then
                                    Html.button [
                                        prop.className "use"
                                        keepFocus
                                        prop.onClick (fun _ ->
                                            let name = (exerciseOnLine line |> Option.defaultValue pick.Exercise)
                                            let row = Printer.printRow { pick with Exercise = name; Note = None }
                                            editInPlace dispatch (fun t c -> Some(Editing.replaceLine t c row)))
                                        prop.text (setText pick)
                                    ]
                            ]
                        ]
                    | None -> ()
                    if not suggestions.IsEmpty then
                        Html.div [
                            prop.className "suggest"
                            prop.children [
                                for s in suggestions do
                                    Html.button [ prop.key s; keepFocus; prop.onClick (fun _ -> editInPlace dispatch (fun t c -> Some(Editing.replaceLine t c $"[{s}] "))); prop.text s ]
                            ]
                        ]
                    Html.div [
                        prop.className "keys"
                        prop.children [
                            let keys =
                                [ ",", "Comma" ]
                                @ (model.Legend |> List.map (fun m -> string m.Symbol, $"Mark: {m.Label}"))
                                @ [ "+", "Added weight"; "-", "Assisted weight"; "BW", "Bodyweight"; " // ", "Note" ]

                            for (k, label) in keys do
                                Html.button [
                                    prop.key k
                                    prop.ariaLabel label
                                    keepFocus
                                    prop.onClick (fun _ -> editInPlace dispatch (fun t c -> Some(Editing.insertAt t c (if k = "BW" then "BW " else k))))
                                    prop.text (if k = " // " then "//" else k)
                                ]
                            Html.button [ prop.className "repeat"; prop.ariaLabel "Repeat last row"; keepFocus; prop.onClick (fun _ ->
                                    match document.getElementById "editor" with
                                    | null -> ()
                                    | el ->
                                        let ta = el :?> Types.HTMLTextAreaElement
                                        if (repeatEdit ta.value (int ta.selectionStart)).IsNone then dispatch (ShowToast "No row above to repeat")
                                        else editInPlace dispatch repeatEdit); prop.children [ repeatIcon ] ]
                        ]
                    ]
                ]
            ]
        ]
    ]

// ---------------------------------------------------------------- history

let private historyView (model: Model) (name: string) dispatch =
    let legend = legendMap model.Legend
    let key = exerciseKey name
    let rows = model.Rows |> List.filter (fun r -> exerciseKey r.Row.Exercise = key)
    let bests = bestsByWeight legend model.Rows name
    let e1rm = bestE1rm legend model.Rows name

    let heaviest =
        rows
        |> List.choose (fun r ->
            match r.Row.Weight with
            | Load w -> Some(w, r)
            | _ -> None)
        |> function
            | [] -> None
            | xs -> Some(List.maxBy fst xs)

    let sessions =
        rows
        |> List.groupBy (fun r -> r.Date, r.NoteId)
        |> List.sortByDescending fst

    let card (k: string) (v: string) (s: string) =
        Html.div [ prop.className "card"; prop.children [ Html.div [ prop.className "k"; prop.text k ]; Html.div [ prop.className "v"; prop.text v ]; Html.div [ prop.className "s"; prop.text s ] ] ]

    Html.div [
        prop.className "screen"
        prop.children [
            Html.div [ prop.className "topbar"; prop.children [ backButton "Back" dispatch ] ]
            Html.h1 [
                prop.className "title"
                prop.style [ style.fontFamily "var(--mono)"; style.fontSize 26 ]
                prop.children [ Html.span [ prop.style [ style.color "var(--faint)" ]; prop.text "[" ]; Html.text name; Html.span [ prop.style [ style.color "var(--faint)" ]; prop.text "]" ] ]
            ]
            Html.div [
                prop.className "scroll"
                prop.children [
                    if rows.IsEmpty then
                        Html.p [ prop.className "empty"; prop.text "No sessions with this exercise yet." ]
                    else
                        Html.div [
                            prop.className "cards"
                            prop.children [
                                match e1rm with
                                | Some(e, r) ->
                                    let best = r.Row.Reps |> List.filter (Scoring.countsForHighs legend) |> List.maxBy (fun x -> x.Count)
                                    card "Best e1RM" (Printer.formatDecimal (Math.Round(decimal e, 1))) $"{weightText r.Row.Weight} × {best.Count} · {formatDate r.Date}"
                                | None -> card "Best e1RM" "—" "needs a plain load"
                                match heaviest with
                                | Some(w, r) -> card "Heaviest" (Printer.formatDecimal (decimal w)) $"× {repsText r.Row.Reps} · {formatDate r.Date}"
                                | None -> card "Heaviest" "—" ""
                            ]
                        ]
                        Html.div [ prop.className "section-label"; prop.text "Best per weight" ]
                        Html.div [
                            prop.className "panel"
                            prop.children [
                                for (w, best, date) in bests do
                                    Html.div [
                                        prop.key (weightText w)
                                        prop.className "brow"
                                        prop.children [
                                            Html.span [ prop.className "w"; prop.text (match weightText w with "" -> "—" | t -> t) ]
                                            Html.span [ prop.text (string best) ]
                                            Html.span [ prop.className "d"; prop.text (formatDate date) ]
                                        ]
                                    ]
                            ]
                        ]
                        Html.div [ prop.className "section-label"; prop.text "By session" ]
                        for ((date, noteId), rs) in sessions do
                            Html.button [
                                prop.key noteId
                                prop.className "note-item sess"
                                prop.onClick (fun _ -> dispatch (OpenNote noteId))
                                prop.children [
                                    Html.div [ prop.className "d"; prop.text (formatDate date) ]
                                    Html.div [ prop.className "l"; prop.text (rs |> List.map (fun r -> setText r.Row) |> String.concat " · ") ]
                                ]
                            ]
                    Html.div [ prop.style [ style.height 40 ] ]
                ]
            ]
        ]
    ]

// ---------------------------------------------------------------- legend

let private legendView (model: Model) dispatch =
    let undefinedMarks =
        Scoring.undefinedMarks (legendMap model.Legend) (model.Rows |> List.map (fun r -> r.Row))

    let behaviours = [ LabelOnly; NoHighs; ExcludeFromCount ]

    let behaviourValue =
        function
        | LabelOnly -> "label"
        | NoHighs -> "nohighs"
        | ExcludeFromCount -> "exclude"

    Html.div [
        prop.className "screen"
        prop.children [
            Html.div [ prop.className "topbar"; prop.children [ backButton "Notes" dispatch ] ]
            Html.h1 [ prop.className "title"; prop.text "Legend" ]
            Html.div [
                prop.className "scroll"
                prop.children [
                    Html.div [ prop.className "section-label"; prop.text "Rep marks" ]
                    Html.div [
                        prop.className "panel"
                        prop.children [
                            for m in model.Legend do
                                Html.div [
                                    prop.key (string m.Symbol)
                                    prop.className "mark"
                                    prop.children [
                                        Html.span [ prop.className "sym"; prop.text (string m.Symbol) ]
                                        Html.div [
                                            prop.children [
                                                Html.input [
                                                    prop.ariaLabel $"Label for {m.Symbol}"
                                                    prop.value m.Label
                                                    prop.onChange (fun (s: string) -> dispatch (SetMarkLabel(m.Symbol, s)))
                                                ]
                                                Html.select [
                                                    prop.ariaLabel $"What {m.Symbol} does"
                                                    prop.value (behaviourValue m.Behaviour)
                                                    prop.onChange (fun (s: string) -> dispatch (SetMarkBehaviour(m.Symbol, behaviourOfString s)))
                                                    prop.children [
                                                        for b in behaviours do
                                                            Html.option [ prop.value (behaviourValue b); prop.text (behaviourName b) ]
                                                    ]
                                                ]
                                            ]
                                        ]
                                        Html.button [ prop.className "icon-btn danger"; prop.ariaLabel $"Remove mark {m.Symbol}"; prop.onClick (fun _ -> dispatch (DeleteMark m.Symbol)); prop.text "×" ]
                                    ]
                                ]
                            Html.div [
                                prop.className "addmark"
                                prop.children [
                                    Html.input [
                                        prop.className "sym"
                                        prop.ariaLabel "New mark symbol"
                                        prop.maxLength 1
                                        prop.placeholder "^"
                                        prop.value model.NewMark
                                        prop.onChange (fun (s: string) -> dispatch (SetNewMark s))
                                    ]
                                    Html.button [ prop.className "link"; prop.onClick (fun _ -> dispatch AddMark); prop.text "Add mark" ]
                                ]
                            ]
                        ]
                    ]
                    if not undefinedMarks.IsEmpty then
                        Html.p [
                            prop.className "note-text"
                            prop.text $"""Used in your notes but not defined: {undefinedMarks |> Seq.map string |> String.concat " "}. Add them above to give them a meaning."""
                        ]
                    Html.div [ prop.className "section-label"; prop.text "Backup" ]
                    Html.div [
                        prop.className "panel"
                        prop.children [
                            Html.div [
                                prop.className "addmark"
                                prop.children [
                                    Html.button [ prop.className "link"; prop.onClick (fun _ -> dispatch ExportNotes); prop.text "Export all notes" ]
                                ]
                            ]
                            Html.div [
                                prop.className "addmark"
                                prop.children [
                                    Html.label [
                                        prop.className "link"
                                        prop.children [
                                            Html.text "Import notes…"
                                            Html.input [
                                                prop.type' "file"
                                                prop.accept ".md,.txt,text/plain,text/markdown"
                                                prop.className "sr-only"
                                                prop.onChange (fun (e: Types.Event) ->
                                                    let input = e.target :?> Types.HTMLInputElement

                                                    if input.files.length > 0 then
                                                        let file = input.files.[0]
                                                        let reader = FileReader.Create()
                                                        reader.onload <- fun _ -> dispatch (ImportNotes(string reader.result))
                                                        reader.readAsText file
                                                        input.value <- "")
                                            ]
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                    Html.p [ prop.className "note-text"; prop.text "Notes are stored on this device only. Export regularly as a backup." ]
                ]
            ]
        ]
    ]

// ---------------------------------------------------------------- root

let view (model: Model) dispatch =
    Html.div [
        prop.children [
            match model.Page with
            | NotesList -> notesListView model dispatch
            | Editor id ->
                match noteById model id with
                | Some note -> editorView model note dispatch
                | None -> notesListView model dispatch
            | ExerciseHistory(name, _) -> historyView model name dispatch
            | LegendPage -> legendView model dispatch
            match model.Toast with
            | Some t -> Html.div [ prop.className "toast"; prop.role "status"; prop.text t ]
            | None -> ()
        ]
    ]
