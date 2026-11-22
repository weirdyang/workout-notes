module Storage

open Fable.Core
open Browser
open WorkoutNotes.Core

type StoredNote =
    { Id: string
      Text: string
      Updated: float }

type private RawNote = {| id: string; text: string; updated: float |}
type private RawMark = {| symbol: string; label: string; behaviour: string |}

let private notesKey = "workout-notes.notes"
let private legendKey = "workout-notes.legend"

let private tryLoad<'T> (key: string) : 'T option =
    try
        match localStorage.getItem key with
        | null -> None
        | s -> Some(JS.JSON.parse s :?> 'T)
    with _ ->
        None

let private save (key: string) (value: obj) =
    try
        localStorage.setItem (key, JS.JSON.stringify value)
    with e ->
        console.error ("Could not save", e)

let newId () =
    string (JS.Constructors.Date.now ()) + "-" + string (int (JS.Math.random () * 1e6))

let loadNotes () : StoredNote list =
    match tryLoad<RawNote[]> notesKey with
    | Some raw ->
        raw
        |> Array.map (fun n ->
            { Id = n.id
              Text = n.text
              Updated = n.updated })
        |> List.ofArray
    | None -> []

let saveNotes (notes: StoredNote list) =
    notes
    |> List.map (fun n -> {| id = n.Id; text = n.Text; updated = n.Updated |})
    |> Array.ofList
    |> box
    |> save notesKey

let private behaviourToString =
    function
    | LabelOnly -> "label"
    | NoHighs -> "nohighs"
    | ExcludeFromCount -> "exclude"

let behaviourOfString =
    function
    | "nohighs" -> NoHighs
    | "exclude" -> ExcludeFromCount
    | _ -> LabelOnly

let defaultLegend: MarkDef list =
    [ { Symbol = '*'; Label = "Ground"; Behaviour = NoHighs }
      { Symbol = '#'; Label = "Paused"; Behaviour = LabelOnly }
      { Symbol = '!'; Label = "Failed"; Behaviour = ExcludeFromCount } ]

let loadLegend () : MarkDef list =
    match tryLoad<RawMark[]> legendKey with
    | Some raw ->
        raw
        |> Array.choose (fun m ->
            if m.symbol.Length = 1 then
                Some
                    { Symbol = m.symbol.[0]
                      Label = m.label
                      Behaviour = behaviourOfString m.behaviour }
            else
                None)
        |> List.ofArray
    | None -> defaultLegend

let saveLegend (legend: MarkDef list) =
    legend
    |> List.map (fun m -> {| symbol = string m.Symbol; label = m.Label; behaviour = behaviourToString m.Behaviour |})
    |> Array.ofList
    |> box
    |> save legendKey

let behaviourName =
    function
    | LabelOnly -> "Label only"
    | NoHighs -> "Doesn't count for highs"
    | ExcludeFromCount -> "Excluded from rep count"

let exportText (notes: StoredNote list) =
    notes |> List.map (fun n -> n.Text.Trim()) |> String.concat "\n\n---\n\n"

let splitImport (text: string) =
    text.Replace("\r\n", "\n").Split([| "\n---\n" |], System.StringSplitOptions.None)
    |> Array.map (fun s -> s.Trim())
    |> Array.filter (fun s -> s <> "")
    |> List.ofArray

[<Emit("navigator.clipboard.writeText($0)")>]
let copyToClipboard (text: string) : JS.Promise<unit> = jsNative

[<Emit("URL.createObjectURL(new Blob([$0], { type: 'text/markdown' }))")>]
let private objectUrl (text: string) : string = jsNative

[<Emit("URL.revokeObjectURL($0)")>]
let private revoke (url: string) : unit = jsNative

let download (fileName: string) (text: string) =
    let url = objectUrl text
    let a = document.createElement "a" :?> Types.HTMLAnchorElement
    a.href <- url
    a.setAttribute ("download", fileName)
    document.body.appendChild a |> ignore
    a.click ()
    document.body.removeChild a |> ignore
    revoke url
