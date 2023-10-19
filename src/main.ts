import { brotli, gzip } from "./compress.ts";
import { Args, parse } from "std/flags/mod.ts";
import { PATHS } from "./constants.ts";
import { Site } from "./site.ts";
import { buildPages, copyPublicFiles, writeAssets } from "./build.ts";
import { httpServer, websocketServer } from "./server.ts";
import { createSitemap } from "./sitemap.ts";
import { startWatcher } from "./watcher.ts";

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

const site = await Site.build(flags.production ? "prod" : "dev");

await buildPages(site.pages, site);
await writeAssets(site.assets);
await copyPublicFiles(site.staticFiles);

if (flags.server && !flags.production) {
  const tx = new BroadcastChannel("tx");
  void startWatcher(site, tx);
  void httpServer();
  void websocketServer(tx);
}

await createSitemap(site);
await gzip("./dist");
await brotli("./dist");
