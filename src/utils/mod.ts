import { createHash } from "std/hash/mod.ts";
import { defaultConfig, setConfig } from "../mod.ts";

export * from "./duration.ts";
export * from "./fs/mod.ts";

/**
 * Convert a title from a page to a slug that can be used in the generated site. Turns
 * `Hello, world!` into `hello-world`.
 *
 * @param title - Title to convert
 * @returns The resulting slug
 */
export const slugify = (title: string): string => {
  return title
    .trim()
    .toLowerCase()
    .split(" ")
    .map((w) => w.replace(/[^\d+a-z]+/gi, ""))
    .join("-")
    .replace(/--+/g, "-");
};

/**
 * Utility to create a hash from the contents of some file.
 *
 * @param content - Content to hash
 * @param production - Whether to generate hash
 * @returns The hash value for the file
 */
export const cacheBust = (content: string, production: boolean): string => {
  if (!production) return "";
  const md5 = createHash("md5");
  md5.update(content);
  return md5.toString().slice(0, 8);
};

/**
 * Like {@link Partial} but works on nested objects too.
 */
export type DeepPartial<T> = {
  [P in keyof T]?: DeepPartial<T[P]>;
};

/**
 * Utility type for when reading something only returns the values as strings.
 */
export type PartialStringyTyped<T> = { [Key in keyof T]: string | undefined };

/**
 * Create a temporary folder in your OS' temporary folder.
 *
 * @param prefix - Prefix of folder
 * @returns Directory path
 */
export const createTestDirectory = (prefix = "test"): string => {
  return Deno.makeTempDirSync({ prefix });
};

/**
 * A simple wrapper around the default config that sets `out` to be a
 * random temporary directory.
 */
export const testConfig = setConfig(defaultConfig, {
  out: createTestDirectory(),
});
