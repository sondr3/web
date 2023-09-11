import { walk } from "std/fs/mod.ts";
import { contentFromPath, renderContent } from "./content.ts";
import { brotli, gzip } from "./compress.ts";
import { buildCSS } from "./asset.ts";
import { PATHS } from "./constants.ts";
import { minifyHTML } from "./minify.ts";
import { createContext } from "./context.ts";

const context = await createContext(PATHS, "dev");
console.log(context);

const css = await buildCSS(PATHS, "prod");

for await (const entry of walk("./site/content/pages", { includeDirs: false })) {
  const content = await contentFromPath(entry.path, "page");
  const rendered = renderContent(content);
  const minified = await minifyHTML(rendered);
  // console.log(minified);
}

await gzip("./dist");
await brotli("./dist");
