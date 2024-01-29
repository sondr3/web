import { assertEquals } from "std/assert/mod.ts";
import * as path from "std/path/mod.ts";
import { PATHS } from "./constants.ts";
import { Path } from "./path.ts";

Deno.test("path.common", () => {
  const it = new Path(path.join(PATHS.public, "./fonts/Piazolla.woff2"));
  const common = it.common(path.join(PATHS.out, "./public"));

  assertEquals(common, `site/public/fonts/Piazolla.woff2`);
});

Deno.test("path.digest", async () => {
  const it = new Path(path.join(PATHS.out, "./test.txt"));
  await it.digest("hello world");
  assertEquals(it.filename, "test.5eb63bbb.txt");
});
