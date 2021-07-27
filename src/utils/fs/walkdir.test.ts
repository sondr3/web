import { assert, assertArrayIncludes } from "std/testing/asserts.ts";
import { walkDirectory } from "./walkdir.ts";

Deno.test("walkDir: JSON files without recursing", async () => {
  const filter = (ext: string) => ext === ".json";
  const files = await walkDirectory("./", filter, false);

  assertArrayIncludes(files, ["import_map.json", "lock.json"]);
});

Deno.test("walkDir: TS files with recursing", async () => {
  const filter = (ext: string) => ext === ".ts";
  const files = await walkDirectory("./src", filter);

  assertArrayIncludes(files, ["src/utils/fs/walkdir.ts"]);
  assertArrayIncludes(files, ["src/utils/fs/walkdir.test.ts"]);
});

Deno.test("walkDir: finds all files in directory, ignoring nothing", async () => {
  const filter = (_: string) => true;
  const files = await walkDirectory("./assets", filter);

  assertArrayIncludes(files, [
    "assets/scss/style.scss",
    "assets/js/livereload.js",
    "assets/images/developer.svg",
  ]);
});

Deno.test("walkDir: finds all files in directory, ignoring scss", async () => {
  const filter = (ext: string) => ext !== ".scss";
  const files = await walkDirectory("./assets", filter);

  assert(!files.includes("assets/scss/style.scss"));
  assertArrayIncludes(files, [
    "assets/js/livereload.js",
    "assets/images/developer.svg",
  ]);
});
