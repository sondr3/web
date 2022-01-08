import { promises as fs } from "node:fs";
import path, { join } from "node:path";
import { EitherAsync } from "purify-ts/EitherAsync.js";

import { cacheBust } from "./utils.js";

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
export const copyFiles = (
  source: string,
  destination: string,
  recurse = true,
): EitherAsync<Error, void> =>
  EitherAsync(async ({ throwE }) => {
    try {
      const entries = await fs.readdir(source, { withFileTypes: true });
      await createDirectory(destination);

      for (const entry of entries) {
        const src = path.join(source, entry.name);
        const dest = path.join(destination, entry.name);

        await (entry.isDirectory() && recurse
          ? copyFiles(src, dest, recurse)
          : fs.copyFile(src, dest, 1));
      }
    } catch (e) {
      throwE(e as Error);
    }
  });

/**
 * A simple wrapper around {@link fs.copyFile}, mostly for error handling purposes.
 *
 * @param source - File to copy
 * @param destination - Destination to copy to
 * @param overwrite - Overwrite the destination? (true by default)
 * @returns Error if something went wrong
 */
export const copyFile = (
  source: string,
  destination: string,
  overwrite = true,
): EitherAsync<Error, void> =>
  EitherAsync(async ({ throwE }) => {
    try {
      await fs.copyFile(source, destination, overwrite ? 0 : 1);
      return;
    } catch (e) {
      return throwE(e as Error);
    }
  });

/**
 * Create a directory for the file whose path is supplied, recursively creating the directories
 * required.
 *
 * @param filepath - Path to where file wants to go
 * @returns Error if something goes wrong
 */
export const createDirectory = (filepath: string): EitherAsync<Error, void> =>
  EitherAsync(async ({ throwE }) => {
    try {
      await fs.mkdir(filepath, { recursive: true });
      return;
    } catch (e) {
      return throwE(e as Error);
    }
  });

/**
 * Writes some content to a file.
 *
 * @param filepath - File to write to
 * @param content - Content to write
 * @returns Error if writing fails
 */
export const writeFile = (filepath: string, content: string | Buffer): EitherAsync<Error, void> =>
  EitherAsync(async ({ throwE }) => {
    try {
      await fs.writeFile(filepath, content);
      return;
    } catch (e) {
      return throwE(e as Error);
    }
  });

/**
 * Reads the content of a file.
 *
 * @param filepath - File to read contents of
 * @returns Error if file could not be read
 */
export const readFile = (filepath: string): EitherAsync<Error, string> =>
  EitherAsync(async ({ throwE }) => {
    try {
      return await fs.readFile(filepath, { encoding: "utf-8" });
    } catch (e) {
      return throwE(e as Error);
    }
  });

/**
 * Remove a directory
 *
 * @param directoryPath - Path to delete
 * @param recursive - Recursively delete all content
 * @param force - Ignore errors
 */
export const rmdir = (
  directory: string,
  recursive = true,
  force = false,
): EitherAsync<Error, void> =>
  EitherAsync(async ({ throwE }) => {
    try {
      return await fs.rm(directory, { recursive, force });
    } catch (e) {
      return throwE(e as Error);
    }
  });

/**
 * Wrapper around {@link rmdir} to delete multiple files.
 *
 * @param toDelete- Paths to delete
 * @param recursive - Recursively delete all content
 * @param force - Ignore errors
 */
export const rmdirs = (
  directories: Array<string>,
  recursive = false,
  force = false,
): EitherAsync<Error, void[]> => {
  return EitherAsync.sequence(directories.map((dir) => rmdir(dir, recursive, force)));
};

/**
 * Read the contents of a file and return its hash.
 *
 * @param file - File to read
 * @returns An eight character long hash
 */
export const hashFile = (file: string, production: boolean): EitherAsync<Error, string> => {
  return readFile(file)
    .map((content) => cacheBust(content, production))
    .mapLeft((e) => e);
};
