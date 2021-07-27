import { prettyPrintDuration } from "./duration.ts";
import { assertEquals } from "std/testing/asserts.ts";

Deno.test("PrettyPrintDuration: pretty formats duration, less than 1 second", () => {
  const duration = 683;
  const formatted = prettyPrintDuration(duration);

  assertEquals(typeof formatted, "string");
  assertEquals(formatted, "683ms");
});

Deno.test("PrettyPrintDuration: pretty formats duration, less than 100ms", () => {
  const duration = 85;
  const formatted = prettyPrintDuration(duration);

  assertEquals(formatted, "85ms");
});

Deno.test("Pretty formats duration, more than 1 second", () => {
  const duration = 1632;
  const formatted = prettyPrintDuration(duration);

  assertEquals(formatted, "1s 632ms");
});
