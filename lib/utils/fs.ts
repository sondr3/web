import { promises as fs } from "fs";
import path from "path";
import crypto from "crypto";
import { logging } from "./logging";

const logger = logging.getLogger("fs");

/**
 * Recursively walk directories finding all files matching the extension.
 *
 * @param directory - Directory to walk
 * @param extension - Extensions to look for
 * @param recurse   - Whether to recursively go into folders
 * @param filepaths - Array of found files
 */
export async function dirWalk(
  directory: string,
  extension: string,
  recurse = true,
  filepaths: Array<string> = [],
): Promise<Array<string>> {
  const files = await fs.readdir(directory);

  for (const filename of files) {
    const filepath = path.join(directory, filename);
    const stat = await fs.stat(filepath);

    if (stat.isDirectory() && recurse) {
      await dirWalk(filepath, extension, recurse, filepaths);
    } else if (path.extname(filename) === `.${extension}`) {
      filepaths.push(filepath);
    }
  }

  return filepaths;
}

export const copyFiles = async (source: string, destination: string, recurse = true): Promise<void | Error> => {
  const entries = await fs.readdir(source, { withFileTypes: true });
  const dir = await createDirectory(destination);
  if (dir) return dir;

  for (const entry of entries) {
    const src = path.join(source, entry.name);
    const dest = path.join(destination, entry.name);

    if (entry.isDirectory() && recurse) {
      await copyFiles(src, dest, recurse);
    } else {
      try {
        await fs.copyFile(src, dest, 1);
      } catch (e) {
        logger.error(e);
        throw e;
      }
    }
  }
};

export const createDirectory = async (filepath: string): Promise<void | Error> => {
  try {
    await fs.mkdir(filepath, { recursive: true });
    return;
  } catch (e) {
    logger.error(e);
    return e as Error;
  }
};

export const writeFile = async (filepath: string, content: string | Buffer): Promise<void | Error> => {
  try {
    await fs.writeFile(filepath, content);
    return;
  } catch (e) {
    logger.error(e);
    throw e;
  }
};

export const cacheBustFile = (contents: string | Buffer, filename: string): string => {
  const md5 = crypto.createHash("md5");
  md5.update(contents);
  const hash = md5.digest("hex").slice(0, 8);
  const { name, ext } = path.parse(filename);

  return `${name}.${hash}${ext}`;
};
