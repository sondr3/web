import { createReadStream, createWriteStream } from "node:fs";
import { promises as stream } from "node:stream";
import { createBrotliCompress, createGzip } from "node:zlib";

import { Context } from "../context.js";
import { Duration } from "../utils/duration.js";
import { walkDir } from "../utils/fs.js";
import * as logger from "../utils/logger.js";
import { Config } from "./config.js";

const filterFiles = (file: string): boolean => {
  return [".gz", ".br", ".png", ".jpg"].every((ext) => !file.endsWith(ext));
};

export const compress = async ({ config }: Context): Promise<void> => {
  if (config.production) {
    await Promise.allSettled([gzip(config), brotli(config)]);
  }
};

/**
 * Compress a directory and all its files with Gzip.
 *
 * @param config - Configuration
 */
export const gzip = async (config: Config): Promise<void> => {
  const gzipDuration = new Duration();
  for await (const file of walkDir(config.out, filterFiles)) {
    const source = createReadStream(file);
    const destination = createWriteStream(`${file}.gz`);
    const gzip = createGzip({ level: 9 });
    await stream.pipeline(source, gzip, destination);
  }

  gzipDuration.end();
  logger.info("Finished gzip compression in", gzipDuration.result());
};

/**
 * Compress a directory and all its files with brotli.
 *
 * @param config - Configuration
 */
export const brotli = async (config: Config): Promise<void> => {
  const brotliCompression = new Duration();
  for await (const file of walkDir(config.out, filterFiles)) {
    const source = createReadStream(file);
    const destination = createWriteStream(`${file}.br`);
    const brotli = createBrotliCompress();
    await stream.pipeline(source, brotli, destination);
  }

  brotliCompression.end();
  logger.info("Finished brotli compression in", brotliCompression.result());
};
