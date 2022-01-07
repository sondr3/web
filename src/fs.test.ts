// import fsSync, { promises as fs } from "node:fs";
import { test } from "@sondr3/minitest";
import { strict as assert } from "node:assert";
// import os from "node:os";
import { extname } from "node:path";

import { walkDir } from "./fs.js";

const walkDirToArray = async (
  dir: string,
  filter: (name: string) => boolean,
  recurse = true,
): Promise<string[]> => {
  const out = [];
  for await (const x of walkDir(dir, filter, recurse)) out.push(x);
  return out;
};

test("walkDir for JSON files", async () => {
  const files = await walkDirToArray(".", (name) => extname(name) === ".json", false);

  assert.deepStrictEqual(files, [`package.json`, `tsconfig.json`]);
});

test("walkDir for TS with recursing", async () => {
  const files = await walkDirToArray("./src", (name) => name.endsWith("ts"), true);

  assert(files.includes("src/fs.ts"));
  assert(files.includes("src/fs.test.ts"));
});
