import { test } from "@sondr3/minitest";
import { strict as assert } from "node:assert";
import { promises as fs } from "node:fs";
import os from "node:os";
import path, { extname } from "node:path";

import { config } from "../build/config.js";
import {
  copyFile,
  copyFiles,
  createDirectory,
  hashFile,
  readFile,
  rmdir,
  rmdirs,
  walkDir,
  writeFile,
} from "./fs.js";

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

test("createDirectory can create a test directory", async () => {
  const actual = await createDirectory("/tmp/testing");
  assert(!(actual instanceof Error));
});

test("createDirectory cannot create a root directory", async () => {
  assert((await createDirectory("/test")) instanceof Error);
});

test("writeFile can create a test file", async () => {
  const directory = await fs.mkdtemp(path.join(os.tmpdir(), "test-"));
  const filename = `${directory}/test.txt`;
  const actual = await writeFile(filename, "hello");
  assert(!(actual instanceof Error));
});

test("writeFile cannot write to /", async () => {
  assert((await writeFile("/test", "hello")) instanceof Error);
});

test("hashFile can add a hash to a file", async () => {
  const actual = await hashFile(path.resolve(process.cwd(), "tsconfig.json"), true);
  assert.equal(actual, "a4d9a208");
});

test("hashFile fails if the file does not exist", async () => {
  assert((await hashFile(path.resolve(process.cwd(), ".hello"), false)) instanceof Error);
});

test("copyFiles copies files without recursing", async () => {
  const dest = await fs.mkdtemp(path.join(os.tmpdir(), "wo-rec"));
  const actual = await copyFiles(config().assets.images, dest, false);
  assert(!(actual instanceof Error));
});

test("copyFiles copies files recursively", async () => {
  const directory = await fs.mkdtemp(path.join(os.tmpdir(), "w-rec"));
  const actual = await copyFiles(path.resolve(process.cwd(), "src"), directory, true);
  assert(!(actual instanceof Error));
});

test("copyFiles throws on illegal directory", async () => {
  assert((await copyFiles("/asdasd", "./")) instanceof Error);
});

test("copyFiles cannot copy wrong files", async () => {
  assert((await copyFiles(path.resolve(process.cwd()), "/")) instanceof Error);
});

test("copyFile copies and overwrites files by default", async () => {
  const directory = await fs.mkdtemp(path.join(os.tmpdir(), "test-copy"));

  let actual = await copyFile(
    path.join(config().assets.root, "robots.txt"),
    path.join(directory, "robots.txt"),
  );
  assert(!(actual instanceof Error));

  const out = await fs.readFile(path.join(directory, "robots.txt"));
  assert(out.includes("Host: https://www.eons.io"));

  actual = await copyFile(
    path.join(config().assets.root, "humans.txt"),
    path.join(directory, "robots.txt"),
  );
  assert(!(actual instanceof Error));

  assert((await fs.readFile(path.join(directory, "robots.txt"))).toString().includes("/* TEAM */"));
});

test("copyFile copies and does not overwrite", async () => {
  const directory = await fs.mkdtemp(path.join(os.tmpdir(), "test-copy2"));
  const actual = await copyFile(
    path.join(config().assets.root, "robots.txt"),
    path.join(directory, "robots.txt"),
  );
  assert(!(actual instanceof Error));

  const result = await copyFile(
    path.join(config().assets.root, "humans.txt"),
    path.join(directory, "robots.txt"),
    false,
  );
  assert(result instanceof Error);
  assert(
    (await fs.readFile(path.join(directory, "robots.txt")))
      .toString()
      .includes("Host: https://www.eons.io"),
  );
});

test("readFile read files", async () => {
  assert(!((await readFile(path.resolve(process.cwd(), "package.json"))) instanceof Error));
});

test("readFile throws when reading unknown file", async () => {
  assert((await readFile(path.resolve(process.cwd(), "poop.json"))) instanceof Error);
});

test("rmdir should delete a directory", async () => {
  const path_ = await fs.mkdtemp("rmdir");
  const result = await rmdir(path_, true);
  assert(!(result instanceof Error));
  await assert.rejects(async () => await fs.access(path_));
});

test("rmdir should fail if a directory does not exist", async () => {
  const path_ = path.resolve(process.cwd(), config().out, "wrongDir");
  const result = await rmdir(path_, true);
  assert(result instanceof Error);
  await assert.rejects(async () => await fs.access(path_));
});

test("rmdirs should delete multiple", async () => {
  const paths = [await fs.mkdtemp("rmdirs"), await fs.mkdtemp("rmdirs")];
  const result = await rmdirs(paths, true);
  assert(!(result instanceof Error));

  paths.map(async (path_) => await assert.rejects(async () => await fs.access(path_)));
});

test("rmdirs should fail if one directory does not exist", async () => {
  const paths = ["js", "wrongDir"].map((path_) => path.resolve(process.cwd(), config().out, path_));
  const result = await rmdirs(paths, true);
  assert(result instanceof Error);

  paths.map(async (path_) => await assert.rejects(async () => await fs.access(path_)));
});
