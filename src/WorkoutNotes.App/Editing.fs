module Editing

let private clamp (text: string) (caret: int) = max 0 (min caret text.Length)

let lineStartAndEnd (text: string) (caret: int) =
    let caret = clamp text caret
    let lineStart = if caret = 0 then 0 else text.LastIndexOf('\n', caret - 1) + 1

    let lineEnd =
        match text.IndexOf('\n', caret) with
        | -1 -> text.Length
        | newline -> newline

    lineStart, lineEnd

let lineIndexAt (text: string) (caret: int) =
    text.Substring(0, clamp text caret) |> Seq.filter ((=) '\n') |> Seq.length

let currentLine (text: string) (caret: int) =
    let lineStart, lineEnd = lineStartAndEnd text caret
    text.Substring(lineStart, lineEnd - lineStart)

let textBeforeCaretOnLine (text: string) (caret: int) =
    let caret = clamp text caret
    let lineStart, _ = lineStartAndEnd text caret
    text.Substring(lineStart, caret - lineStart)

let insertAt (text: string) (caret: int) (insert: string) =
    let caret = clamp text caret
    text.Substring(0, caret) + insert + text.Substring(caret), caret + insert.Length

let replaceLine (text: string) (caret: int) (newLine: string) =
    let lineStart, lineEnd = lineStartAndEnd text caret
    text.Substring(0, lineStart) + newLine + text.Substring(lineEnd), lineStart + newLine.Length

let mapLinesBelowHeader (mapLine: string -> string) (text: string) =
    match text.Split('\n') |> List.ofArray with
    | header :: body -> header :: (body |> List.map mapLine) |> String.concat "\n"
    | [] -> text
