import { assert, assertEquals } from "std/testing/asserts.ts";
import * as path from "std/path/mod.ts";

import { defaultConfig, setConfig } from "./config.ts";

Deno.test("config: sets correct default settings", () => {
  assert(defaultConfig.out.includes("/test"));
  assertEquals(
    defaultConfig.content.pages,
    path.join(path.resolve(Deno.cwd()), "content/pages/"),
  );
});

Deno.test("config: can be overridden", () => {
  const config = setConfig(defaultConfig, {
    out: "./public",
    production: true,
    url: "http://test.com",
  });

  assert(config.out.includes("/public"));
  assert(config.production);
  assertEquals(config.url, "http://test.com");
});
