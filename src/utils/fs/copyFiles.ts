import * as log from "std/log/mod.ts";
import * as path from "std/path/mod.ts";
import { FSError } from "./mod.ts";

const logger = log.getLogger();

/**
 * Copies all files from a source directory to a destination, optionally recursively
 * copying the subdirectories as well.
 *
 * @param source - Where to copy from
 * @param destination - Where to copy to
 * @param recurse - Whether to recursively copy
 * @returns Error if something went wrong
 */
export const copyFiles = async (
  source: string,
  destination: string,
  recurse = true,
) => {
  try {
    await Deno.mkdir(destination, { recursive: true });

    for await (const entry of Deno.readDir(source)) {
      const src = path.join(source, entry.name);
      const dest = path.join(destination, entry.name);

      if (entry.isDirectory && recurse) await copyFiles(src, dest, recurse);
      else await Deno.copyFile(src, dest);
    }
  } catch ({ message }) {
    logger.error(message);
    throw new FSError(message);
  }
};
