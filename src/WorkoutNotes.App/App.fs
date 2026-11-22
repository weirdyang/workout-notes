module App

open System
open Fable.Core
open Elmish
open Feliz
open Browser
open WorkoutNotes.Core
open WorkoutNotes.Core.History
open Storage

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
      EditorHasFocus: bool
      HelpOpen: bool
      Toast: string option }

type Msg =
    | OpenNote of string
    | NewNote
    | Back
    | TextChanged of string * int
    | CaretMoved of int
    | EditorFocused
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
    | OpenHelp
    | CloseHelp
    | ShowToast of string
    | ClearToast

let legendMap (legend: MarkDef list) : Legend =
    legend |> List.map (fun m -> m.Symbol, m) |> Map.ofList

let today () =
    let n = DateTime.Now
    DateOnly(n.Year, n.Month, n.Day)

let private tryParseStoredNote (n: StoredNote) =
    match Parser.parseNote n.Text with
    | Ok note -> Some note
    | Error _ -> None

let private recomputeFromNotes (reparseOnlyNoteIds: string list option) (model: Model) =
    let parsed =
        match reparseOnlyNoteIds with
        | None -> model.Notes |> List.map (fun n -> n.Id, tryParseStoredNote n) |> Map.ofList
        | Some ids ->
            let keep = model.Notes |> List.map (fun n -> n.Id) |> Set.ofList
            let current = model.Parsed |> Map.filter (fun id _ -> keep.Contains id)

            model.Notes
            |> List.filter (fun n -> List.contains n.Id ids || not (current.ContainsKey n.Id))
            |> List.fold (fun acc n -> Map.add n.Id (tryParseStoredNote n) acc) current

    let rows =
        parsed |> Map.toSeq |> Seq.choose (fun (id, n) -> n |> Option.map (fun n -> id, n)) |> rowsOf

    { model with
        Parsed = parsed
        Rows = rows
        Badges = allBadges (legendMap model.Legend) rows }

let private convertBracketRowsToAtFormat (notes: StoredNote list) =
    let upgraded =
        notes
        |> List.map (fun n ->
            let text =
                n.Text.Split('\n')
                |> Array.map (fun l -> if l.Trim().StartsWith "[" then Printer.tidyLine l else l)
                |> String.concat "\n"

            { n with Text = text })

    if upgraded <> notes then saveNotes upgraded
    upgraded

let init () =
    { Notes = loadNotes () |> convertBracketRowsToAtFormat
      Parsed = Map.empty
      Rows = []
      Badges = Map.empty
      Legend = loadLegend ()
      Page = NotesList
      Search = ""
      Caret = 0
      NewMark = ""
      EditorHasFocus = false
      HelpOpen = false
      Toast = None }
    |> recomputeFromNotes None,
    Cmd.none

let private focusEditorAt (caret: int) : Cmd<Msg> =
    Cmd.ofEffect (fun _ ->
        JS.setTimeout
            (fun () ->
                match document.getElementById "editor" with
                | null -> ()
                | element ->
                    let editor = element :?> Types.HTMLTextAreaElement
                    editor.focus ()
                    editor.setSelectionRange (caret, caret))
            0
        |> ignore)

let private showToast (text: string) : Cmd<Msg> =
    Cmd.ofMsg (ShowToast text)

let private tryFindNote (model: Model) id =
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

    { model with
        Notes = notes
        Caret = min model.Caret text.Length }
    |> recomputeFromNotes (Some [ id ])

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
    | Unweighted -> ""
    | w -> Printer.printWeight w

let repsText (reps: Rep list) = Printer.printReps reps

let weightAndRepsText (row: ExerciseRow) = Printer.printWeightAndReps row

let private bodyLinesWithLineNumbers (text: string) =
    let lines = text.Split('\n')
    let body = lines |> Array.skip (min 1 lines.Length)

    let skipped =
        body |> Array.takeWhile (fun l -> l.Trim() = "") |> Array.length

    body
    |> Array.skip skipped
    |> Array.mapi (fun i l -> i, 1 + skipped + i, l)

let private exerciseOnLine (line: string) =
    match Parser.parseLine line with
    | Exercise r -> Some r.Exercise
    | _ ->
        match Parser.nameOnLine line with
        | "" -> None
        | n -> Some n

let private hasNameButNoSets (line: string) =
    let t = line.Trim()
    let i = t.IndexOf '@'
    t <> "" && (i < 0 || t.Substring(i + 1).Trim() = "")

let private editDistance (a: string) (b: string) =
    let mutable prev = Array.init (b.Length + 1) id
    for i in 1 .. a.Length do
        let cur = Array.zeroCreate (b.Length + 1)
        cur.[0] <- i
        for j in 1 .. b.Length do
            let cost = if a.[i - 1] = b.[j - 1] then 0 else 1
            cur.[j] <- min (min (prev.[j] + 1) (cur.[j - 1] + 1)) (prev.[j - 1] + cost)
        prev <- cur
    prev.[b.Length]

let private closestKnownExercise (exercisesInOtherRows: string list) (name: string) =
    let k = exerciseKey name

    if k = "" || exercisesInOtherRows |> List.exists (fun n -> exerciseKey n = k) then
        None
    else
        exercisesInOtherRows
        |> List.map (fun n -> n, editDistance (exerciseKey n) k)
        |> List.filter (fun (n, d) -> d <= max 2 (k.Length / 3))
        |> List.sortBy snd
        |> List.tryHead
        |> Option.map fst

let private repeatPreviousRow (text: string) (caret: int) =
    let lineNo = Editing.lineIndexAt text caret
    let lines = text.Split('\n')

    lines
    |> Array.take (min lineNo lines.Length)
    |> Array.rev
    |> Array.tryPick (fun l ->
        match Parser.parseLine l with
        | Exercise r -> Some(Printer.printRow r)
        | _ -> None)
    |> Option.map (fun row ->
        if (Editing.currentLine text caret).Trim() = "" then
            Editing.replaceLine text caret row
        else
            let _, e = Editing.lineStartAndEnd text caret
            Editing.insertAt text e ("\n" + row))

let private applyEditKeepingCaret (dispatch: Msg -> unit) (edit: string -> int -> (string * int) option) =
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

let private withOpenNote (model: Model) f =
    match model.Page with
    | Editor id ->
        match tryFindNote model id with
        | Some note -> f id note.Text
        | None -> model, Cmd.none
    | _ -> model, Cmd.none

let private removeNoteIfEmpty (model: Model) id =
    match tryFindNote model id with
    | Some n when n.Text.Split('\n') |> Array.skip 1 |> Array.forall (fun l -> l.Trim() = "") ->
        let notes = model.Notes |> List.filter (fun x -> x.Id <> id)
        saveNotes notes
        { model with Notes = notes } |> recomputeFromNotes (Some [])
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
        |> recomputeFromNotes (Some [ id ]),
        focusEditorAt text.Length

    | Back ->
        match model.Page with
        | Editor id ->
            let m = withOpenNote model (fun id text -> updateNoteText id (Editing.mapLinesBelowHeader Printer.tidyLine text) model, Cmd.none) |> fst
            { removeNoteIfEmpty m id with Page = NotesList }, Cmd.none
        | ExerciseHistory(_, back) -> { model with Page = back }, Cmd.none
        | LegendPage
        | NotesList -> { model with Page = NotesList }, Cmd.none

    | TextChanged(text, caret) ->
        withOpenNote model (fun id _ -> { updateNoteText id text model with Caret = caret }, Cmd.none)

    | CaretMoved caret -> { model with Caret = caret }, Cmd.none

    | EditorFocused -> { model with EditorHasFocus = true }, Cmd.none

    | EditorBlurred ->
        let model = { model with EditorHasFocus = false }

        withOpenNote model (fun id text ->
            let normalized = Editing.mapLinesBelowHeader Printer.tidyLine text
            if normalized = text then model, Cmd.none else updateNoteText id normalized model, Cmd.none)

    | CopyNote ->
        withOpenNote model (fun _ text ->
            model,
            Cmd.batch
                [ Cmd.ofEffect (fun _ -> copyToClipboard (Editing.mapLinesBelowHeader Printer.tidyLine text) |> ignore)
                  showToast "Copied" ])

    | DeleteNote ->
        withOpenNote model (fun id _ ->
            if window.confirm "Delete this note?" then
                let notes = model.Notes |> List.filter (fun n -> n.Id <> id)
                saveNotes notes
                { model with Notes = notes; Page = NotesList } |> recomputeFromNotes (Some []), Cmd.none
            else
                model, Cmd.none)

    | OpenHistory name ->
        let model =
            match model.Page with
            | Editor id -> { (withOpenNote model (fun id text -> updateNoteText id (Editing.mapLinesBelowHeader Printer.tidyLine text) model, Cmd.none) |> fst) with Page = model.Page }
            | _ -> model

        { model with Page = ExerciseHistory(name, model.Page) }, Cmd.none

    | OpenLegend -> { model with Page = LegendPage }, Cmd.none
    | SetSearch s -> { model with Search = s }, Cmd.none

    | ExportNotes ->
        let d = today ()
        let name = $"workout-notes-{d.Year}-{d.Month:D2}-{d.Day:D2}.md"
        let ordered = model.Notes |> List.sortBy (fun n -> n.Updated)
        download name (exportText ordered)
        model, showToast $"Exported {model.Notes.Length} notes"

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
        { model with Notes = notes } |> recomputeFromNotes None, showToast $"Imported {added.Length} notes"

    | SetMarkLabel(sym, label) ->
        let legend = model.Legend |> List.map (fun m -> if m.Symbol = sym then { m with Label = label } else m)
        saveLegend legend
        { model with Legend = legend }, Cmd.none

    | SetMarkBehaviour(sym, b) ->
        let legend = model.Legend |> List.map (fun m -> if m.Symbol = sym then { m with Behaviour = b } else m)
        saveLegend legend
        { model with Legend = legend } |> recomputeFromNotes (Some []), Cmd.none

    | DeleteMark sym ->
        let legend = model.Legend |> List.filter (fun m -> m.Symbol <> sym)
        saveLegend legend
        { model with Legend = legend } |> recomputeFromNotes (Some []), Cmd.none

    | SetNewMark s -> { model with NewMark = s }, Cmd.none

    | AddMark ->
        let s = model.NewMark.Trim()

        if s.Length <> 1 then
            model, showToast "A mark is one character"
        elif not (Parser.isMarkChar s.[0]) then
            model, showToast $"'{s}' can't be a mark"
        elif model.Legend |> List.exists (fun m -> m.Symbol = s.[0]) then
            model, showToast $"'{s}' is already defined"
        else
            let legend = model.Legend @ [ { Symbol = s.[0]; Label = "New mark"; Behaviour = LabelOnly } ]
            saveLegend legend
            { model with Legend = legend; NewMark = "" } |> recomputeFromNotes (Some []), Cmd.none

    | OpenHelp -> { model with HelpOpen = true }, Cmd.none
    | CloseHelp -> { model with HelpOpen = false }, Cmd.none

    | ShowToast t ->
        { model with Toast = Some t },
        Cmd.ofEffect (fun dispatch -> JS.setTimeout (fun () -> dispatch ClearToast) 1800 |> ignore)
    | ClearToast -> { model with Toast = None }, Cmd.none

let private icon (svg: string) =
    Html.span [ prop.style [ style.display.inlineFlex ]; prop.ariaHidden true; prop.dangerouslySetInnerHTML svg ]

let private chevron =
    icon """<svg width="22" height="22" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2.2" stroke-linecap="round" stroke-linejoin="round"><path d="M15 18l-6-6 6-6"/></svg>"""

let private plus =
    icon """<svg width="26" height="26" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2.4" stroke-linecap="round"><path d="M12 5v14M5 12h14"/></svg>"""

let private gear =
    icon """<svg width="22" height="22" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><path d="M4 7h10M18 7h2M4 17h4M12 17h8"/><circle cx="16" cy="7" r="2"/><circle cx="10" cy="17" r="2"/></svg>"""

let private infoIcon =
    icon """<svg width="22" height="22" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round"><circle cx="12" cy="12" r="9"/><path d="M12 11v6"/><circle cx="12" cy="7.5" r="0.6" fill="currentColor"/></svg>"""

let private infoButton dispatch =
    Html.button [
        prop.className "icon-btn"
        prop.ariaLabel "How to log"
        prop.onClick (fun _ -> dispatch OpenHelp)
        prop.children [ infoIcon ]
    ]

let private repeatIcon =
    icon """<svg width="20" height="20" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2.2" stroke-linecap="round" stroke-linejoin="round"><path d="M17 2l4 4-4 4"/><path d="M3 11v-1a4 4 0 0 1 4-4h14"/><path d="M7 22l-4-4 4-4"/><path d="M21 13v1a4 4 0 0 1-4 4H3"/></svg>"""

let private trash =
    icon """<svg width="20" height="20" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><path d="M3 6h18M8 6V4h8v2M6 6l1 14h10l1-14"/></svg>"""

let private backButton (label: string) dispatch =
    Html.button [ prop.className "link"; prop.onClick (fun _ -> dispatch Back); prop.children [ chevron; Html.text label ] ]

let private doesNotStealEditorFocus = prop.onMouseDown (fun e -> e.preventDefault ())

let private badgeView (b: Badge option) =
    match b with
    | Some LoadHigh -> Html.span [ prop.className "badge"; prop.text "L+"; prop.title "Load high" ]
    | Some RepHigh -> Html.span [ prop.className "badge"; prop.text "R+"; prop.title "Rep high" ]
    | Some Tie -> Html.span [ prop.className "badge tie"; prop.text "="; prop.title "Ties your best" ]
    | None -> Html.span []

let private chip (name: string) dispatch =
    Html.button [
        prop.className "chip"
        doesNotStealEditorFocus
        prop.onClick (fun _ -> dispatch (OpenHistory name))
        prop.text name
    ]

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
                    infoButton dispatch
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
                        Html.p [ prop.className "empty"; prop.text "No notes yet. Tap + to log today's session — one line per exercise, like: Incline @ 85x6,6,6" ]
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

let private editorView (model: Model) (note: StoredNote) dispatch =
    let text = note.Text
    let caretLine = Editing.lineIndexAt text model.Caret
    let line = Editing.currentLine text model.Caret
    let before = Editing.textBeforeCaretOnLine text model.Caret
    let parsedNote = Map.tryFind note.Id model.Parsed |> Option.flatten
    let noteDate = parsedNote |> Option.map (fun n -> n.Header.Date) |> Option.defaultValue (today ())
    let body = bodyLinesWithLineNumbers text

    let caretRowIndex =
        body |> Array.tryPick (fun (i, lineNo, _) -> if lineNo = caretLine then Some i else None)

    let exercisesInOtherRows =
        model.Rows
        |> List.filter (fun r -> not (r.NoteId = note.Id && Some r.Index = caretRowIndex))
        |> exerciseNamesMostRecentFirst

    let nameSuggestions =
        if caretLine = 0 || before.Contains "@" || before.Trim().StartsWith "//" then
            []
        else
            let q = exerciseKey before

            exercisesInOtherRows
            |> List.filter (fun n -> q = "" || (exerciseKey n).Contains q)
            |> List.filter (fun n -> exerciseKey n <> q)
            |> List.truncate 8

    let currentName = if caretLine = 0 then None else exerciseOnLine line

    let didYouMean =
        match currentName with
        | Some name when line.Contains "@" -> closestKnownExercise exercisesInOtherRows name |> Option.map (fun k -> name, k)
        | _ -> None

    let isNew =
        match currentName with
        | Some name when line.Contains "@" && not exercisesInOtherRows.IsEmpty ->
            not (exercisesInOtherRows |> List.exists (fun k -> exerciseKey k = exerciseKey name))
        | _ -> false

    let lastSessionHint =
        currentName
        |> Option.bind (fun name ->
            lastSession model.Rows name noteDate note.Id
            |> Option.map (fun (date, rows) ->
                let above =
                    text.Split('\n')
                    |> Array.take caretLine
                    |> Array.skip 1
                    |> Array.filter (fun l ->
                        match Parser.parseLine l with
                        | Exercise r -> exerciseKey r.Exercise = exerciseKey name
                        | _ -> false)
                    |> Array.length

                let pick = rows.[min above (rows.Length - 1)]
                name, date, rows, pick))

    let currentBadge =
        match parsedNote, caretRowIndex with
        | Some _, Some i -> Map.tryFind (note.Id, i) model.Badges
        | _ -> None

    let replaceName (newName: string) (t: string) (c: int) =
        let l = Editing.currentLine t c
        let at = l.IndexOf '@'
        let rest = if at >= 0 then l.Substring(at) else "@ "
        Some(Editing.replaceLine t c $"{newName} {rest.TrimEnd()} ")

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
                            infoButton dispatch
                            Html.button [ prop.className "icon-btn"; prop.ariaLabel "Delete note"; prop.onClick (fun _ -> dispatch DeleteNote); prop.children [ trash ] ]
                            Html.button [ prop.className "link strong"; prop.onClick (fun _ -> dispatch CopyNote); prop.text "Copy" ]
                        ]
                    ]
                ]
            ]
            if parsedNote.IsNone then
                Html.div [ prop.className "perr"; prop.text "First line must start with a date, like 2026-11-22 push" ]
            Html.div [
                prop.className "editor-wrap"
                prop.children [
                    Html.textarea [
                        prop.id "editor"
                        prop.className "editor"
                        prop.ariaLabel "Note"
                        prop.placeholder "incline @ 85x6,6,6"
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
                        prop.onFocus (fun _ -> dispatch EditorFocused)
                        prop.onBlur (fun _ -> dispatch EditorBlurred)
                    ]
                    if not model.EditorHasFocus then
                        Html.div [
                            prop.className "parsed"
                            prop.children [
                                for (i, lineNo, raw) in body do
                                    match Parser.parseLine raw with
                                    | Exercise r ->
                                        let b = if parsedNote.IsSome then Map.tryFind (note.Id, i) model.Badges else None

                                        Html.div [
                                            prop.key lineNo
                                            prop.className "prow"
                                            prop.children [
                                                chip r.Exercise dispatch
                                                Html.span [ prop.className "w"; prop.text (weightAndRepsText r) ]
                                                Html.span [ prop.className "b"; prop.children [ badgeView b ] ]
                                            ]
                                        ]
                                    | Unparsed(rawLine, _) ->
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
            if model.EditorHasFocus then
                Html.div [
                    prop.className "keybar"
                    prop.children [
                        match didYouMean with
                        | Some(typed, suggestion) ->
                            Html.div [
                                prop.className "hint"
                                prop.children [
                                    Html.span [ prop.text $"New exercise “{typed}”. Did you mean" ]
                                    Html.button [
                                        prop.className "use"
                                        doesNotStealEditorFocus
                                        prop.onClick (fun _ -> applyEditKeepingCaret dispatch (replaceName suggestion))
                                        prop.text suggestion
                                    ]
                                ]
                            ]
                        | None ->
                            match lastSessionHint with
                            | Some(_, date, rows, pick) ->
                                Html.div [
                                    prop.className "hint"
                                    prop.children [
                                        Html.span [
                                            prop.className "hint-text"
                                            prop.children [
                                                badgeView currentBadge
                                                Html.text $"""Last · {formatDate date} · {rows |> List.map weightAndRepsText |> String.concat " · "}"""
                                            ]
                                        ]
                                        if hasNameButNoSets line then
                                            Html.button [
                                                prop.className "use"
                                                doesNotStealEditorFocus
                                                prop.onClick (fun _ ->
                                                    let name = exerciseOnLine line |> Option.defaultValue pick.Exercise
                                                    let row = Printer.printRow { pick with Exercise = name; Note = None }
                                                    applyEditKeepingCaret dispatch (fun t c -> Some(Editing.replaceLine t c row)))
                                                prop.text (weightAndRepsText pick)
                                            ]
                                    ]
                                ]
                            | None when isNew ->
                                Html.div [ prop.className "hint"; prop.children [ Html.span [ prop.text "New exercise" ] ] ]
                            | None ->
                                match currentBadge with
                                | Some _ -> Html.div [ prop.className "hint"; prop.children [ badgeView currentBadge ] ]
                                | None -> ()
                        if not nameSuggestions.IsEmpty then
                            Html.div [
                                prop.className "suggest"
                                prop.children [
                                    for s in nameSuggestions do
                                        Html.button [
                                            prop.key s
                                            doesNotStealEditorFocus
                                            prop.onClick (fun _ -> applyEditKeepingCaret dispatch (fun t c -> Some(Editing.replaceLine t c $"{s} @ ")))
                                            prop.text s
                                        ]
                                ]
                            ]
                        Html.div [
                            prop.className "keys"
                            prop.children [
                                let keys =
                                    [ "@", "At: weight follows"; "x", "Times: reps follow"; ",", "Next set" ]
                                    @ (model.Legend |> List.map (fun m -> string m.Symbol, $"Mark: {m.Label}"))
                                    @ [ "bw", "Bodyweight" ]

                                for (k, label) in keys do
                                    Html.button [
                                        prop.key k
                                        prop.ariaLabel label
                                        doesNotStealEditorFocus
                                        prop.onClick (fun _ ->
                                            applyEditKeepingCaret dispatch (fun t c ->
                                                let insert =
                                                    match k with
                                                    | "@" -> if c > 0 && t.[c - 1] = ' ' then "@ " else " @ "
                                                    | "bw" -> "bw "
                                                    | k -> k
                                                Some(Editing.insertAt t c insert)))
                                        prop.text k
                                    ]
                                Html.button [
                                    prop.className "repeat"
                                    prop.ariaLabel "Repeat last row"
                                    doesNotStealEditorFocus
                                    prop.onClick (fun _ ->
                                        match document.getElementById "editor" with
                                        | null -> ()
                                        | el ->
                                            let ta = el :?> Types.HTMLTextAreaElement
                                            if (repeatPreviousRow ta.value (int ta.selectionStart)).IsNone then dispatch (ShowToast "No row above to repeat")
                                            else applyEditKeepingCaret dispatch repeatPreviousRow)
                                    prop.children [ repeatIcon ]
                                ]
                            ]
                        ]
                    ]
                ]
        ]
    ]

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
            | Load(w, u) -> Some((u = Parser.defaultUnit, w), r)
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
                prop.text name
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
                                    card "Best e1RM" (Printer.formatDecimal (Math.Round(e, 1))) $"{weightText r.Row.Weight}x{best.Count} · {formatDate r.Date}"
                                | None -> card "Best e1RM" "—" "needs a plain load"
                                match heaviest with
                                | Some(_, r) -> card "Heaviest" (weightText r.Row.Weight) $"x{repsText r.Row.Reps} · {formatDate r.Date}"
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
                                    Html.div [ prop.className "l"; prop.text (rs |> List.map (fun r -> weightAndRepsText r.Row) |> String.concat " · ") ]
                                ]
                            ]
                    Html.div [ prop.style [ style.height 40 ] ]
                ]
            ]
        ]
    ]

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

let private helpView (model: Model) dispatch =
    let example (code: string) (meaning: string) =
        Html.div [
            prop.className "help-row"
            prop.children [ Html.code code; Html.span meaning ]
        ]

    let section (title: string) (rows: ReactElement list) =
        Html.div [
            prop.className "help-section"
            prop.children [ Html.h3 title; yield! rows ]
        ]

    Html.div [
        prop.className "sheet-backdrop"
        prop.onClick (fun _ -> dispatch CloseHelp)
        prop.children [
            Html.div [
                prop.className "sheet"
                prop.role "dialog"
                prop.ariaLabel "How to log"
                prop.onClick (fun e -> e.stopPropagation ())
                prop.children [
                    Html.div [
                        prop.className "sheet-head"
                        prop.children [
                            Html.h2 "How to log"
                            Html.button [ prop.className "link strong"; prop.onClick (fun _ -> dispatch CloseHelp); prop.text "Done" ]
                        ]
                    ]
                    section "One line per exercise" [
                        example "incline @ 85x6,6,6" "exercise @ weight x reps"
                        example "incline@85 6,6,6" "spaces and the x are optional"
                        example "incline @ 92.5x3 // easy" "a note after //"
                        example "legs cooked" "a line without @ is just text"
                    ]
                    section "Weight" [
                        example "incline @ 85x6" "85 kg (no unit means kg)"
                        example "curl @ 40lbx10" "unit right after the number: kg, lb, plates…"
                        example "pull-up @ +16x5" "+ added weight"
                        example "dip @ -18x10" "- then a number: assisted"
                        example "leg raise @ bw 20" "bw: bodyweight"
                        example "plank @ - 60,60" "- on its own: no weight"
                    ]
                    section "Badges" [
                        Html.div [ prop.className "help-row"; prop.children [ badgeView (Some LoadHigh); Html.span "Heaviest ever for this exercise (same unit)" ] ]
                        Html.div [ prop.className "help-row"; prop.children [ badgeView (Some RepHigh); Html.span "Most reps ever at this weight" ] ]
                        Html.div [ prop.className "help-row"; prop.children [ badgeView (Some Tie); Html.span "Ties your best at this weight" ] ]
                    ]
                    section "Marks" [
                        for mark in model.Legend do
                            example $"8{mark.Symbol}" $"{mark.Label} · {behaviourName mark.Behaviour}"
                        Html.p [ prop.className "help-note"; prop.text "Edit marks in Legend (the sliders icon on Notes)." ]
                    ]
                    section "While typing" [
                        Html.p [ prop.className "help-note"; prop.text "Pick a name from the suggestions, tap last session's set to fill it, and use the bar for @ x , marks and bw so you stay on the number keyboard. Rows are tidied when you leave the note." ]
                    ]
                ]
            ]
        ]
    ]

let view (model: Model) dispatch =
    Html.div [
        prop.children [
            match model.Page with
            | NotesList -> notesListView model dispatch
            | Editor id ->
                match tryFindNote model id with
                | Some note -> editorView model note dispatch
                | None -> notesListView model dispatch
            | ExerciseHistory(name, _) -> historyView model name dispatch
            | LegendPage -> legendView model dispatch
            if model.HelpOpen then helpView model dispatch
            match model.Toast with
            | Some t -> Html.div [ prop.className "toast"; prop.role "status"; prop.text t ]
            | None -> ()
        ]
    ]
