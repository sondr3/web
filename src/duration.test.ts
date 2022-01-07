import { test } from "@sondr3/minitest";
import { strict as assert } from "node:assert";

import { prettyPrintDuration } from "./duration.js";

test("PrettyPrintDuration, less than 1 second", () => {
  const duration = 683;
  const formatted = prettyPrintDuration(duration);

  assert.equal(typeof formatted, "string");
  assert.equal(formatted, "683ms");
});

test("PrettyPrintDuration, less than 100ms", () => {
  const duration = 85;
  const formatted = prettyPrintDuration(duration);

  assert.equal(formatted, "85ms");
});

test("PrettyPrintDuration, more than 1 second", () => {
  const duration = 1632;
  const formatted = prettyPrintDuration(duration);

  assert.equal(formatted, "1s 632ms");
});
