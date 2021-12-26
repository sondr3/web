import { cacheBust, slugify } from "./mod.ts";
import { assertEquals } from "asserts";

Deno.test("slugify", () => {
  const tests = [
    ["Hello, world! It's a glorious", "hello-world-its-a-glorious"],
    ["this_ IS a % of $dollars", "this-is-a-of-dollars"],
    ["1 is equal == to ---3", "1-is-equal-to-3"],
  ];

  tests.forEach((inputs) => {
    assertEquals(slugify(inputs[0]), inputs[1]);
  });
});

Deno.test("cacheBust", async () => {
  assertEquals(await cacheBust("content", false), "");
  assertEquals(await cacheBust("content", true), "9a0364b9");
});
