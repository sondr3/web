import { promises as fs } from "fs";
import path from "path";
import crypto from "crypto";
import { EitherAsync, liftPromise } from "purify-ts/EitherAsync";

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

export const createDirectory = (filepath: string): EitherAsync<Error, string> =>
  liftPromise(() => fs.mkdir(filepath, { recursive: true })).mapLeft(({ message }) =>
    Error(`Could not create directory: ${message}`),
  );

export const cacheBustFile = (contents: string | Buffer, filename: string): string => {
  const md5 = crypto.createHash("md5");
  md5.update(contents);
  const hash = md5.digest("hex").slice(0, 8);
  const { name, ext } = path.parse(filename);

  return `${name}.${hash}${ext}`;
};
