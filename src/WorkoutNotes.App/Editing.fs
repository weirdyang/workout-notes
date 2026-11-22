/// Pure text operations on the editor's content and caret.
module Editing

/// Start and end (exclusive) of the line containing `pos`.
let lineBounds (text: string) (pos: int) =
    let pos = max 0 (min pos text.Length)
    let start = if pos = 0 then 0 else text.LastIndexOf('\n', pos - 1) + 1
    let e = text.IndexOf('\n', pos)
    start, (if e < 0 then text.Length else e)

let lineIndexAt (text: string) (pos: int) =
    text.Substring(0, max 0 (min pos text.Length)) |> Seq.filter ((=) '\n') |> Seq.length

let currentLine (text: string) (pos: int) =
    let s, e = lineBounds text pos
    text.Substring(s, e - s)

/// Text of the current line up to the caret.
let beforeCaret (text: string) (pos: int) =
    let pos = max 0 (min pos text.Length)
    let s, _ = lineBounds text pos
    text.Substring(s, max 0 (pos - s))

let insertAt (text: string) (pos: int) (insert: string) =
    let pos = max 0 (min pos text.Length)
    text.Substring(0, pos) + insert + text.Substring(pos), pos + insert.Length

/// Replace the whole line containing `pos`; caret goes to the end of the new line.
let replaceLine (text: string) (pos: int) (newLine: string) =
    let s, e = lineBounds text pos
    text.Substring(0, s) + newLine + text.Substring(e), s + newLine.Length

/// Normalise every line except the header (first line).
let normalizeAll (normalize: string -> string) (text: string) =
    match text.Split('\n') |> List.ofArray with
    | header :: rest -> header :: (rest |> List.map normalize) |> String.concat "\n"
    | [] -> text
