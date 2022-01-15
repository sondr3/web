import { test } from "@sondr3/minitest";
import { strict as assert } from "node:assert";
import { promises as fs } from "node:fs";
import os from "node:os";
import path, { extname } from "node:path";

import { config } from "../build/config.js";
import { copyFiles, walkDir } from "./fs.js";

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

  assert(files.includes("package.json"));
  assert(files.includes("tsconfig.json"));
});

test("walkDir for TS with recursing", async () => {
  const files = await walkDirToArray("./src", (name) => name.endsWith("ts"), true);

  assert(files.includes("src/utils/fs.ts"));
  assert(files.includes("src/utils/fs.test.ts"));
});

test("copyFiles copies files without recursing", async () => {
  const dest = await fs.mkdtemp(path.join(os.tmpdir(), "wo-rec"));
  assert.doesNotThrow(async () => await copyFiles(config().assets.images, dest));
});

test("copyFiles copies files recursively", async () => {
  const dest = await fs.mkdtemp(path.join(os.tmpdir(), "w-rec"));
  assert.doesNotThrow(async () => await copyFiles(config().assets.images, dest));
});
