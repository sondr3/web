import { createHash } from "std/hash/mod.ts";
import * as log from "std/log/mod.ts";
import { FSError } from "./mod.ts";

const logger = log.getLogger();

/**
 * Create a hash of a file, based on its content. The hash is an eight character long
 * MD5 hash used by reading the files contents.
 *
 * @param filepath - File to read and hash
 * @returns The hash of the file
 */
export const createFileHash = async (filepath: string): Promise<string> => {
  try {
    const content = await Deno.readFile(filepath);
    const md5 = createHash("md5");
    md5.update(content);
    return md5.toString().slice(0, 8);
  } catch ({ message }) {
    logger.error(message);
    throw new FSError(message);
  }
};
