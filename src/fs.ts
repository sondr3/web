import { promises as fs } from "node:fs";
import path, { join } from "node:path";

import { errOrThrow } from "./utils.js";

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
): Promise<void | Error> => {
  try {
    const entries = await fs.readdir(source, { withFileTypes: true });
    await createDirectory(destination);

    for (const entry of entries) {
      const source_ = path.join(source, entry.name);
      const destination_ = path.join(destination, entry.name);

      await (entry.isDirectory() && recurse
        ? copyFiles(source_, destination_, recurse)
        : fs.copyFile(source_, destination_, 1));
    }
  } catch (e) {
    return errOrThrow(e);
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
): Promise<void | Error> => {
  try {
    await fs.copyFile(source, destination, overwrite ? 0 : 1);
  } catch (e) {
    return errOrThrow(e);
  }
};

/**
 * Create a directory for the file whose path is supplied, recursively creating the directories
 * required.
 *
 * @param filepath - Path to where file wants to go
 * @returns Error if something goes wrong
 */
export const createDirectory = async (filepath: string): Promise<void | Error> => {
  try {
    await fs.mkdir(filepath, { recursive: true });
  } catch (e) {
    return errOrThrow(e);
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
): Promise<void | Error> => {
  try {
    await fs.writeFile(filepath, content);
  } catch (e) {
    return errOrThrow(e);
  }
};

/**
 * Reads the content of a file.
 *
 * @param filepath - File to read contents of
 * @returns Error if file could not be read
 */
export const readFile = async (filepath: string): Promise<string | Error> => {
  try {
    return await fs.readFile(filepath, { encoding: "utf-8" });
  } catch (e) {
    return errOrThrow(e);
  }
};

/**
 * Remove a directory
 *
 * @param directoryPath - Path to delete
 * @param recursive - Recursively delete all content
 * @param force - Ignore errors
 */
export const rmdir = async (
  directory: string,
  recursive = true,
  force = false,
): Promise<void | Error> => {
  try {
    await fs.rm(directory, { recursive, force });
    return;
  } catch (e) {
    return errOrThrow(e);
  }
};

/**
 * Wrapper around {@link rmdir} to delete multiple files.
 *
 * @param toDelete- Paths to delete
 * @param recursive - Recursively delete all content
 * @param force - Ignore errors
 */
export const rmdirs = async (
  directories: Array<string>,
  recursive = false,
  force = false,
): Promise<void | Error> => {
  for (const dir of directories) {
    const res = await rmdir(dir, recursive, force);
    if (res instanceof Error) return res;
  }
};
