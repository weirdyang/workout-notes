import { defineConfig } from "vite";

// The F# app is compiled by Fable into src/WorkoutNotes.App/fable_output, then bundled by Vite.
export default defineConfig({
  root: "src/WorkoutNotes.App",
  base: "./",
  build: { outDir: "../../dist", emptyOutDir: true },
  server: { host: true, port: 5173 },
});
