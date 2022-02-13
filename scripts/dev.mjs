import watcher from "@parcel/watcher";
import { execSync } from "node:child_process";
import { create } from "browser-sync";
import { parse } from "node:path";

console.log("Server listening on http://localhost:3000");
const bs = create();
bs.init({
  server: "./build",
});

async function watch() {
  try {
    await watcher.subscribe("site", async (err, events) => {
      if (err !== null) throw err;
      for (const { path } of events) {
        const e = parse(path);
        if ([".css", ".map"].some((p) => e.ext === p)) continue;
        console.info(`${e.base} changed, rebuilding...`);
        execSync("cabal run");
        bs.reload();
      }
    });
  } catch (e) {
    console.error(e);
  }
}

console.log("Starting watcher");
void watch();
