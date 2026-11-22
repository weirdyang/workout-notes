# Workout Notes

Apple Notes for workout logging: one plain-text note per session, one line per exercise.

```
2026-11-21 Sat · Incline + legs

incline @ 92.5x3
incline @ 85x6,6,6
pull-up @ +16x4,4,3#
curl @ 40lbx10,10
hanging leg raise @ bw 20,20,20*
legs cooked by the end
```

Row: `exercise @ weight x reps // note`. A line is a row only if it has `@`.

- Weight: `85` (kg by default), `85kg`, `40lb`, `2plates` — unit letters right after the number; `+16` added, `-18` assisted, `bw` bodyweight, `-` none.
- `x` or a space separates weight and reps; spaces are optional: `incline@85x3,3,3`.
- Reps are comma-separated, each with an optional user-defined mark (`8*`, `8#`).
- Weights only compare within the same unit. Old `[exercise] weight reps` lines are converted automatically.

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
- Type `incline @ 85x6,6,6`. The key bar has `@`, `x`, `,`, your marks and `bw`, so after the name you stay on the number keyboard.
- Start typing a name to pick from your exercises; the bar shows last session's sets and one tap fills the matching one.
- A name you've never used is flagged as new, with the closest existing name offered ("did you mean incline?").
- Rows are tidied when you leave the note (`incline@85kg x 3` → `incline @ 85x3`).
- Badges: `R+` rep high, `L+` load high, `=` ties your best. Marks are defined in Legend.
- Copy puts the note on the clipboard (for pasting into a chat). Notes stay on the device; export from Legend.

## Hosting

Every push to `main` runs `.github/workflows/deploy.yml`: tests, builds the app, and deploys `dist/` to Cloudflare Workers
static assets (`wrangler.jsonc`), at **https://log.wdym.fyi**.

Deploy needs two repository secrets (Settings → Secrets and variables → Actions):

- `CLOUDFLARE_API_TOKEN` — account: Workers Admin (or Workers Scripts Write); zone `wdym.fyi`: Workers Routes Write
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
