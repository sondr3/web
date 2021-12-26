import { prettyPrintDuration } from "./duration.ts";
import { assertEquals } from "https://deno.land/std@0.119.0/testing/asserts.ts";

Deno.test("PrettyPrintDuration", async (t) => {
  await t.step("less than 1 second", () => {
    const duration = 683;
    const formatted = prettyPrintDuration(duration);

    assertEquals(typeof formatted, "string");
    assertEquals(formatted, "683ms");
  });

  await t.step("less than 100ms", () => {
    const duration = 85;
    const formatted = prettyPrintDuration(duration);

    assertEquals(formatted, "85ms");
  });

  await t.step("more than 1 second", () => {
    const duration = 1632;
    const formatted = prettyPrintDuration(duration);

    assertEquals(formatted, "1s 632ms");
  });
});
