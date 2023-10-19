import { compressFolder } from "./compress.ts";
import { Args, parse } from "std/flags/mod.ts";
import * as log from "std/log/mod.ts";
import { PATHS } from "./constants.ts";
import { Site } from "./site.ts";
import { Server } from "./server.ts";
import { createSitemap } from "./sitemap.ts";
import { Watcher } from "./watcher.ts";

const HELP = `
web - website generator

Options:
  -s, --server      Disable dev server
  -p, --production  Optimize output
  -v, --verbose     Verbose output
  -h, --help        This message
  
Environment variables:
  CI,PROD           Optimize output
`.trim();

interface Flags extends Args {
  server: boolean;
  production: boolean;
  verbose: boolean;
  help: boolean;
}

const flags = parse(Deno.args, {
  boolean: ["server", "production", "verbose", "help"],
  default: { server: !Deno.env.has("CI"), production: Deno.env.has("CI"), verbose: false, help: false },
  alias: { s: "server", v: "verbose", p: "production", h: "help" },
}) as Flags;

if (flags.help) {
  console.log(HELP);
  Deno.exit(0);
}

try {
  await Deno.remove(PATHS.out, { recursive: true });
} catch { /* noop */ }

log.setup({
  handlers: {
    console: new log.handlers.ConsoleHandler(flags.verbose ? "DEBUG" : "INFO"),
  },
});

const site = await Site.create(flags.production ? "prod" : "dev");

await site.write();
await createSitemap(site.content.values());

if (flags.server && !flags.production) {
  const tx = new BroadcastChannel("tx");
  void new Watcher(site, tx).start();
  void new Server(tx).start();
}

if (site.isProd()) {
  await compressFolder(PATHS.out);
}
