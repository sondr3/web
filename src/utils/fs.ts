import { promises as fs } from "node:fs";
import { join } from "node:path";

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
 * @returns Error if something went wrong
 */
export const copyFiles = async (source: string, destination: string): Promise<void> => {
  await fs.mkdir(destination, { recursive: true });
  await fs.cp(source, destination, { recursive: true });
};
