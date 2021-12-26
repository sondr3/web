import * as path from "path/mod.ts";

/**
 * Recursively walk directories finding all files matching the extension.
 */
export async function walkDirectory(
  directory: string,
  filter: (ext: string) => boolean,
  recurse = true,
  filepaths: Array<string> = [],
): Promise<Array<string>> {
  for await (const dirEntry of Deno.readDir(directory)) {
    const filepath = path.join(directory, dirEntry.name);

    if (dirEntry.isDirectory && recurse) {
      await walkDirectory(filepath, filter, recurse, filepaths);
    } else if (filter(path.extname(dirEntry.name))) {
      filepaths.push(filepath);
    }
  }

  return filepaths;
}
