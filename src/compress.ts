import { extname } from "std/path/extname.ts";
import { walk, WalkEntry } from "std/fs/walk.ts";
import { brotliCompressSync } from "node:zlib";

const VALID_EXTENSIONS: Array<string> = [
  ".html",
  ".css",
  ".js",
  ".xml",
  ".css",
  ".cjs",
  ".mjs",
  ".json",
  ".txt",
  ".svg",
  ".map",
  ".ttf",
  ".otf",
  ".woff2",
  ".eot",
];

const is_compressible = (file: WalkEntry): boolean => {
  return VALID_EXTENSIONS.some((e) => e == extname(file.name));
};

export const compress_folder = async (dir: string): Promise<void> => {
  await gzip(dir);
  await brotli(dir);
};

export const gzip = async (dir: string): Promise<void> => {
  for await (const file of walk(dir, { includeDirs: false })) {
    if (!is_compressible(file)) continue;

    const source = await Deno.open(file.path, { read: true });
    const dest = await Deno.open(`${file.path}.gz`, { create: true, truncate: true, write: true });

    source.readable
      .pipeThrough(new CompressionStream("gzip"))
      .pipeTo(dest.writable);
  }
};

const transformer = {
  start() {},
  transform(chunk: Uint8Array, controller: TransformStreamDefaultController) {
    controller.enqueue(brotliCompressSync(chunk));
  },
};

class BrotliCompressionStream extends TransformStream {
  constructor() {
    super({ ...transformer });
  }
}

export const brotli = async (dir: string): Promise<void> => {
  for await (const file of walk(dir, { includeDirs: false })) {
    if (!is_compressible(file)) continue;

    const source = await Deno.open(file.path, { read: true });
    const dest = await Deno.open(`${file.path}.br`, { create: true, truncate: true, write: true });

    source.readable
      .pipeThrough(new BrotliCompressionStream())
      .pipeTo(dest.writable);
  }
};
