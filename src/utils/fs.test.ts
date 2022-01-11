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
  assert((await createDirectory("/tmp/testing").run()).isRight());
});

test("createDirectory cannot create a root directory", async () => {
  assert((await createDirectory("/test").run()).isLeft());
});

test("writeFile can create a test file", async () => {
  const directory = await fs.mkdtemp(path.join(os.tmpdir(), "test-"));
  const filename = `${directory}/test.txt`;
  assert((await writeFile(filename, "hello").run()).isRight());
});

test("writeFile cannot write to /", async () => {
  assert((await writeFile("/test", "hello").run()).isLeft());
});

test("hashFile can add a hash to a file", async () => {
  const actual = await hashFile(path.resolve(process.cwd(), "tsconfig.json"), true).run();
  assert.equal(actual.extract(), "a4d9a208");
});

test("hashFile fails if the file does not exist", async () => {
  assert((await hashFile(path.resolve(process.cwd(), ".hello"), false).run()).isLeft());
});

test("copyFiles copies files without recursing", async () => {
  const dest = await fs.mkdtemp(path.join(os.tmpdir(), "wo-rec"));
  assert((await copyFiles(config().assets.images, dest, false)).isRight());
});

test("copyFiles copies files recursively", async () => {
  const directory = await fs.mkdtemp(path.join(os.tmpdir(), "w-rec"));
  assert((await copyFiles(path.resolve(process.cwd(), "src"), directory, true).run()).isRight());
});

test("copyFiles throws on illegal directory", async () => {
  assert((await copyFiles("/asdasd", "./").run()).isLeft());
});

test("copyFiles cannot copy wrong files", async () => {
  assert((await copyFiles(path.resolve(process.cwd()), "/").run()).isLeft());
});

test("copyFile copies and overwrites files by default", async () => {
  const directory = await fs.mkdtemp(path.join(os.tmpdir(), "test-copy"));

  let actual = await copyFile(
    path.join(config().assets.root, "robots.txt"),
    path.join(directory, "robots.txt"),
  ).run();
  assert(actual.isRight());

  const out = await fs.readFile(path.join(directory, "robots.txt"));
  assert(out.includes("Host: https://www.eons.io"));

  actual = await copyFile(
    path.join(config().assets.root, "humans.txt"),
    path.join(directory, "robots.txt"),
  ).run();
  assert(actual.isRight());

  assert((await fs.readFile(path.join(directory, "robots.txt"))).toString().includes("/* TEAM */"));
});

test("copyFile copies and does not overwrite", async () => {
  const directory = await fs.mkdtemp(path.join(os.tmpdir(), "test-copy2"));
  const actual = await copyFile(
    path.join(config().assets.root, "robots.txt"),
    path.join(directory, "robots.txt"),
  ).run();
  assert(actual.isRight());

  const result = await copyFile(
    path.join(config().assets.root, "humans.txt"),
    path.join(directory, "robots.txt"),
    false,
  ).run();
  assert(result.isLeft());
  assert(
    (await fs.readFile(path.join(directory, "robots.txt")))
      .toString()
      .includes("Host: https://www.eons.io"),
  );
});

test("readFile read files", async () => {
  assert((await readFile(path.resolve(process.cwd(), "package.json")).run()).isRight());
});

test("readFile throws when reading unknown file", async () => {
  assert((await readFile(path.resolve(process.cwd(), "poop.json")).run()).isLeft());
});

test("rmdir should delete a directory", async () => {
  const path_ = await fs.mkdtemp("rmdir");
  const result = await rmdir(path_, true).run();
  assert(result.isRight());
  await assert.rejects(async () => await fs.access(path_));
});

test("rmdir should fail if a directory does not exist", async () => {
  const path_ = path.resolve(process.cwd(), config().out, "wrongDir");
  const result = await rmdir(path_, true).run();
  assert(result.isLeft());
  await assert.rejects(async () => await fs.access(path_));
});

test("rmdirs should delete multiple", async () => {
  const paths = [await fs.mkdtemp("rmdirs"), await fs.mkdtemp("rmdirs")];
  const result = await rmdirs(paths, true);
  assert(result.isRight());

  paths.map(async (path_) => await assert.rejects(async () => await fs.access(path_)));
});

test("rmdirs should fail if one directory does not exist", async () => {
  const paths = ["js", "wrongDir"].map((path_) => path.resolve(process.cwd(), config().out, path_));
  const result = await rmdirs(paths, true);
  assert(result.isLeft());

  paths.map(async (path_) => await assert.rejects(async () => await fs.access(path_)));
});
