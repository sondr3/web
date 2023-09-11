import { assertEquals } from "std/assert/assert_equals.ts";
import { digestFilename } from "./utils.ts";

Deno.test("digestFilename", async () => {
  const digest = await digestFilename("test.txt", "hello world");
  assertEquals(digest, "test.5eb63bbb.txt");
});
