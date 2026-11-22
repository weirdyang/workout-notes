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
| `src/WorkoutNotes.Core/History.fs` | Last session, best per weight, badges (rep high, load high, tie) |
| `src/WorkoutNotes.App` | The web app: Fable + Elmish + Feliz, bundled by Vite, installable as a PWA |
| `tests/WorkoutNotes.Core.Tests` | xUnit examples + FsCheck round-trip property |

## Run

```
dotnet test              # core library tests
dotnet tool restore
npm install
npm run dev              # app with live reload on http://localhost:5173
npm run build            # static site in dist/
```

Targets .NET 10. The app needs Node.js; Fable compiles the F# to JavaScript.

## Using the app

- One note per session; the first line is the date (`2026-11-22 Sun · Push`).
- Type rows without brackets (`Incline 85 6,6,6`); Enter wraps the name: `[Incline] 85 6,6,6`.
- Start typing a name to pick from your exercises; the bar shows last session's sets and fills the matching one.
- Badges: `R+` rep high, `L+` load high, `=` ties your best. Marks are defined in Legend.
- Copy puts the note on the clipboard (for pasting into a chat). Notes stay on the device; export from Legend.

## Hosting

Every push to `main` runs `.github/workflows/deploy.yml`: tests, builds the app, and deploys `dist/` to Cloudflare Workers
static assets (`wrangler.jsonc`), at `https://workout-notes.<your-subdomain>.workers.dev`.

Deploy needs two repository secrets (Settings → Secrets and variables → Actions):

- `CLOUDFLARE_API_TOKEN` — a token made from the "Edit Cloudflare Workers" template
- `CLOUDFLARE_ACCOUNT_ID` — from the Cloudflare dashboard

Without them the workflow still tests and builds, and skips the deploy.

## Scripts

Double-click from Explorer. Each writes its output to `scripts/logs/<name>.log`.

| Script | Does |
| --- | --- |
| `scripts/test.cmd` | `dotnet test` |
| `scripts/build.cmd` | `dotnet build` |
| `scripts/restore.cmd` | `dotnet restore` |
| `scripts/clean.cmd` | `dotnet clean` |
| `scripts/dev.cmd` | Run the app locally with live reload (needs Node.js) |
| `scripts/publish.cmd` | Build the app into `dist/` |
