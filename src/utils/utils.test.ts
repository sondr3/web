import { test } from "@sondr3/minitest";
import { strict as assert } from "node:assert";

import { cacheBust, slugify } from "./utils.js";

test("slugify", () => {
  const tests = [
    ["Hello, world! It's a glorious", "hello-world-its-a-glorious"],
    ["this_ IS a % of $dollars", "this-is-a-of-dollars"],
    ["1 is equal == to ---3", "1-is-equal-to-3"],
  ];
  tests.forEach(([input, expected]) => {
    assert.equal(slugify(input), expected);
  });
});

test("cacheBust", () => {
  assert.equal(cacheBust("content", false), "");
  assert.equal(cacheBust("content", true), "9a0364b9");
});
