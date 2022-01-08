import crypto from "node:crypto";

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
export const cacheBust = (content: string | Buffer, production: boolean): string => {
  if (!production) return "";
  const md5 = crypto.createHash("md5");
  md5.update(content);
  return md5.digest("hex").slice(0, 8);
};

export const errOrThrow = (err: unknown): Error => {
  if (!(err instanceof Error)) throw new Error("err was not an error ???");

  return err;
};
