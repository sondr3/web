import { brotli, gzip } from "./compress.ts";
import { Args, parse } from "std/flags/mod.ts";
import { PATHS } from "./constants.ts";
import { createContext } from "./context.ts";
import { buildPages, copyPublicFiles, writeAssets } from "./build.ts";
import { httpServer, websocketServer } from "./server.ts";
import { createSitemap } from "./sitemap.ts";

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

try {
  await Deno.remove(PATHS.out, { recursive: true });
} catch { /* noop */ }

if (flags.help) {
  console.log(HELP);
  Deno.exit(0);
}

const context = await createContext(PATHS, flags.production ? "prod" : "dev");

await buildPages(context.pages, context.mode, context.assets);
await writeAssets(context.assets);
await copyPublicFiles(context.public_files);

if (flags.server && !flags.production) {
  void httpServer();
  void websocketServer();
}

await createSitemap(context);
await gzip("./dist");
await brotli("./dist");
