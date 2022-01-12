import crypto from "node:crypto";

/**
 * Log and coerce a caught thing to an error;
 * @param e - Something that should be an error
 */
export const logErr = (e: unknown): Error => {
  if (process.env.NODE_ENV !== "test") console.error(e);
  return e as Error;
};

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
