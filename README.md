# Workout Notes

Apple Notes for workout logging: one plain-text note per session, one line per exercise.

```
2026-11-21 Sat · Incline + legs

[Incline]        92.5   3
[Incline]        85     6,6,6
[Pull-up]        +16    4,4,3#
[Hanging leg raise]  BW  20,20,20*
legs cooked by the end
```

Row grammar: `[exercise] weight reps // note`. Weight is `72.5`, `+16` (added), `-18` (assisted), `BW`, or omitted.
Reps are comma-separated, each with an optional user-defined mark (`8*`, `8#`).

## Layout

| Path | What |
| --- | --- |
| `src/WorkoutNotes.Core/Domain.fs` | Types: `Weight`, `Rep`, `ExerciseRow`, `Line`, `Note`, legend |
| `src/WorkoutNotes.Core/Parser.fs` | XParsec row parser (pure F#, Fable-compatible), header and note parsing. Never throws |
| `src/WorkoutNotes.Core/Printer.fs` | Back to text; `parseLine (printRow r) = Exercise r` |
| `src/WorkoutNotes.Core/Scoring.fs` | Mark behaviours, Epley e1RM, undefined-mark check |
| `tests/WorkoutNotes.Core.Tests` | xUnit examples + FsCheck round-trip property |

## Run

```
dotnet test
```

Targets .NET 10.

## Scripts

Double-click from Explorer. Each writes its output to `scripts/logs/<name>.log`.

| Script | Does |
| --- | --- |
| `scripts/test.cmd` | `dotnet test` |
| `scripts/build.cmd` | `dotnet build` |
| `scripts/restore.cmd` | `dotnet restore` |
| `scripts/clean.cmd` | `dotnet clean` |
