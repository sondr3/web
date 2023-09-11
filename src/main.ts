import { brotli, gzip } from "./compress.ts";
import { PATHS } from "./constants.ts";
import { createContext } from "./context.ts";
import { buildPages, copyPublicFiles, writeAssets } from "./build.ts";
import { handleHttp } from "./server.ts";

try {
  await Deno.remove(PATHS.out, { recursive: true });
} catch { /* noop */ }

const context = await createContext(PATHS, "dev");

await buildPages(context.pages, context.mode, context.assets);
await writeAssets(context.assets);
await copyPublicFiles(context.public_files);

if (context.mode === "dev") {
  const server = Deno.listen({ port: 3000 });
  console.log("File server running on http://localhost:3000/");

  for await (const conn of server) {
    handleHttp(conn).catch(console.error);
  }
}

await gzip("./dist");
await brotli("./dist");
