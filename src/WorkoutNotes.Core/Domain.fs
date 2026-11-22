namespace WorkoutNotes.Core

open System

type Weight =
    | Load of value: decimal * unit: string
    | Added of value: decimal * unit: string
    | Assisted of value: decimal * unit: string
    | Bodyweight
    | Unweighted

type Rep = { Count: int; Mark: char option }

type ExerciseRow =
    { Exercise: string
      Weight: Weight
      Reps: Rep list
      Note: string option }

type Line =
    | Exercise of ExerciseRow
    | Text of string
    | Blank
    | Unparsed of raw: string * error: string

type Header = { Date: DateOnly; Title: string option }

type Note = { Header: Header; Lines: Line list }

type Behaviour =
    | LabelOnly
    | NoHighs
    | ExcludeFromCount

type MarkDef =
    { Symbol: char
      Label: string
      Behaviour: Behaviour }

type Legend = Map<char, MarkDef>
