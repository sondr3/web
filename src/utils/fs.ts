import { promises as fs } from "node:fs";
import path, { join } from "node:path";

import { cacheBust, logErr } from "./utils.js";

/**
 * Recursively walk a directory yielding all files matching the filter.
 *
 * @param directory - Directory to walk
 * @param filter - Function to filter file names
 * @param recurse - Terminate recursion
 * @returns A matching file
 */
export async function* walkDir(
  directory: string,
  filter: (name: string) => boolean,
  recurse = true,
): AsyncGenerator<string> {
  for await (const dir of await fs.opendir(directory)) {
    const entry = join(directory, dir.name);
    if (dir.isDirectory() && recurse) {
      yield* walkDir(entry, filter, recurse);
    } else if (dir.isFile() && filter(dir.name)) {
      yield entry;
    }
  }
}

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
): Promise<Error | void> => {
  try {
    const entries = await fs.readdir(source, { withFileTypes: true });
    await createDirectory(destination);

    for (const entry of entries) {
      const src = path.join(source, entry.name);
      const dest = path.join(destination, entry.name);

      await (entry.isDirectory() && recurse
        ? copyFiles(src, dest, recurse)
        : copyFile(src, dest, true));
    }
  } catch (e) {
    return logErr(e);
  }
};

/**
 * A simple wrapper around {@link fs.copyFile}, mostly for error handling purposes.
 *
 * @param source - File to copy
 * @param destination - Destination to copy to
 * @param overwrite - Overwrite the destination? (true by default)
 * @returns Error if something went wrong
 */
export const copyFile = async (
  source: string,
  destination: string,
  overwrite = true,
): Promise<Error | void> => {
  try {
    await fs.copyFile(source, destination, overwrite ? 0 : 1);
    return;
  } catch (e) {
    return logErr(e);
  }
};

/**
 * Create a directory for the file whose path is supplied, recursively creating the directories
 * required.
 *
 * @param filepath - Path to where file wants to go
 * @returns Error if something goes wrong
 */
export const createDirectory = async (filepath: string): Promise<Error | void> => {
  try {
    await fs.mkdir(filepath, { recursive: true });
    return;
  } catch (e) {
    return logErr(e);
  }
};

/**
 * Writes some content to a file.
 *
 * @param filepath - File to write to
 * @param content - Content to write
 * @returns Error if writing fails
 */
export const writeFile = async (
  filepath: string,
  content: string | Buffer,
): Promise<Error | void> => {
  try {
    await fs.writeFile(filepath, content);
    return;
  } catch (e) {
    return logErr(e);
  }
};

/**
 * Reads the content of a file.
 *
 * @param filepath - File to read contents of
 * @returns Error if file could not be read
 */
export const readFile = async (filepath: string): Promise<Error | string> => {
  try {
    return await fs.readFile(filepath, { encoding: "utf-8" });
  } catch (e) {
    return logErr(e);
  }
};

/**
 * Remove a directory
 *
 * @param directory - Path to delete
 * @param recursive - Recursively delete all content
 * @param force - Ignore errors
 */
export const rmdir = async (
  directory: string,
  recursive = true,
  force = false,
): Promise<Error | void> => {
  try {
    return await fs.rm(directory, { recursive, force });
  } catch (e) {
    return logErr(e);
  }
};

/**
 * Wrapper around {@link rmdir} to delete multiple files.
 *
 * @param directories - Paths to delete
 * @param recursive - Recursively delete all content
 * @param force - Ignore errors
 */
export const rmdirs = async (
  directories: Array<string>,
  recursive = false,
  force = false,
): Promise<Error | void> => {
  const res = await Promise.all(directories.map(async (dir) => await rmdir(dir, recursive, force)));
  if (res.some((r) => r instanceof Error)) return new Error("could not remove dirs");

  return;
};

/**
 * Read the contents of a file and return its hash.
 *
 * @param file - File to read
 * @param production - Are we in production mode?
 * @returns An eight character long hash
 */
export const hashFile = async (file: string, production: boolean): Promise<Error | string> => {
  const content = await readFile(file);
  if (content instanceof Error) return content;
  return cacheBust(content, production);
};
