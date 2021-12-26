import { assert, assertArrayIncludes } from "asserts";
import { walkDirectory } from "./walkdir.ts";

Deno.test("WalkDir", async (t) => {
  await t.step("JSON files without recursing", async () => {
    const filter = (ext: string) => ext === ".json";
    const files = await walkDirectory("./", filter, false);

    assertArrayIncludes(files, ["deno.json", "lock.json"]);
  });

  await t.step("TS files with recursing", async () => {
    const filter = (ext: string) => ext === ".ts";
    const files = await walkDirectory("./src", filter);

    assertArrayIncludes(files, ["src/utils/fs/walkdir.ts"]);
    assertArrayIncludes(files, ["src/utils/fs/walkdir.test.ts"]);
  });

  await t.step("finds all files in directory, ignoring nothing", async () => {
    const filter = (_: string) => true;
    const files = await walkDirectory("./static", filter);

    assertArrayIncludes(files, [
      "static/favicon.ico",
      "static/robots.txt",
      "static/Piazzolla.woff2",
    ]);
  });

  await t.step("finds all files in directory, ignoring scss", async () => {
    const filter = (ext: string) => ext !== ".woff2";
    const files = await walkDirectory("./static", filter);

    assert(!files.includes("static/Piazzolla.woff2"));
    assertArrayIncludes(files, [
      "static/favicon.ico",
      "static/robots.txt",
    ]);
  });
});
