namespace WorkoutNotes.Core

open System

[<Measure>]
type kg

/// The weight column of a row. Each form only compares with its own form.
type Weight =
    | Load of decimal<kg>      // 72.5
    | Added of decimal<kg>     // +16  (weighted pull-up, dip)
    | Assisted of decimal<kg>  // -18  (assisted machine)
    | Bodyweight               // BW
    | Unweighted               // omitted (machines with no number, etc.)

/// One rep count with an optional user-defined mark, e.g. 8 or 8*.
type Rep = { Count: int; Mark: char option }

/// `[exercise] weight reps // note`
type ExerciseRow =
    { Exercise: string
      Weight: Weight
      Reps: Rep list
      Note: string option }

/// Every line of a note is one of these. Nothing the user typed is ever dropped.
type Line =
    | Exercise of ExerciseRow
    | Text of string
    | Blank
    | Unparsed of raw: string * error: string

type Header = { Date: DateOnly; Title: string option }

type Note = { Header: Header; Lines: Line list }

/// What the app does with a rep that carries a given mark.
type Behaviour =
    | LabelOnly         // shown and searchable, no effect on maths
    | NoHighs           // counts toward volume, can't set or tie a rep high
    | ExcludeFromCount  // logged, but left out of volume and highs

type MarkDef =
    { Symbol: char
      Label: string
      Behaviour: Behaviour }

type Legend = Map<char, MarkDef>
