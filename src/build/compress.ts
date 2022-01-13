import { createReadStream, createWriteStream } from "node:fs";
import { promises as stream } from "node:stream";
import { createBrotliCompress, createGzip } from "node:zlib";

import { walkDir } from "../utils/fs.js";
import { Config } from "./config.js";

const filterFiles = (file: string): boolean => {
  return [".gz", ".br", ".png", ".jpg"].every((ext) => !file.endsWith(ext));
};

export const compress = async (config: Config): Promise<void> => {
  if (config.production) {
    await Promise.allSettled([gzip(config), brotli(config)]);
  }
};

/**
 * Compress a directory and all its files with Gzip.
 *
 * @param config - Sitewide configuration
 */
export const gzip = async (config: Config): Promise<void> => {
  for await (const file of walkDir(config.out, filterFiles)) {
    const source = createReadStream(file);
    const destination = createWriteStream(`${file}.gz`);
    const gzip = createGzip({ level: 9 });
    await stream.pipeline(source, gzip, destination);
  }
};

/**
 * Compress a directory and all its files with brotli.
 *
 * @param config - Sitewide configuration
 */
export const brotli = async (config: Config): Promise<void> => {
  for await (const file of walkDir(config.out, filterFiles)) {
    const source = createReadStream(file);
    const destination = createWriteStream(`${file}.br`);
    const brotli = createBrotliCompress();
    await stream.pipeline(source, brotli, destination);
  }
};
