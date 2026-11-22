import { defineConfig } from "vite";

export default defineConfig({
  root: "src/WorkoutNotes.App",
  base: "./",
  build: { outDir: "../../dist", emptyOutDir: true },
  server: { host: true, port: 5173 },
});
