import { assertThrowsAsync } from "std/testing/asserts.ts";
import * as path from "std/path/mod.ts";
import { testConfig } from "../mod.ts";
import { copyFiles, FSError } from "./mod.ts";

Deno.test("copyFiles: copies files without recursing", async () => {
  const config = testConfig;
  await copyFiles(
    config.assets.images,
    path.join(config.out, "images"),
    false,
  );
});

Deno.test("copyFiles: copies files recursively", async () => {
  const directory = await Deno.makeTempDir({ prefix: "test-" });
  await copyFiles(path.resolve(Deno.cwd(), "src"), directory, true);
});

Deno.test("copyFiles: crashes on illegal directory", () => {
  assertThrowsAsync(
    () => copyFiles("/asdasd", "./"),
    FSError,
    "No such file or directory",
  );
});

Deno.test("copyFiles: cannot copy wrong files", () => {
  assertThrowsAsync(
    () => copyFiles(path.resolve(Deno.cwd()), "/"),
    FSError,
    "Permission denied",
  );
});
