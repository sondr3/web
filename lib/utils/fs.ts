import { promises as fs } from "fs";
import path from "path";
import { Either, EitherAsync } from "purify-ts";

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

export const createDirectory = async (filepath: string): Promise<Either<Error, void>> =>
  EitherAsync(async ({ throwE }) => {
    try {
      await fs.mkdir(filepath, { recursive: true });
    } catch (e) {
      const err = e as NodeJS.ErrnoException;

      throwE(Error(`Could not create directory: ${err.message}`));
    }
  });

createDirectory("/test")
  .then((res) => console.log(res))
  .catch((err) => console.error(err));
