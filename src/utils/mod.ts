import { crypto } from "crypto/mod.ts";

export * from "./duration.ts";

/**
 * Convert a title from a page to a slug that can be used in the generated site. Turns
 * `Hello, world!` into `hello-world`.
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
 */
export const cacheBust = async (content: string, production: boolean): Promise<string> => {
  if (!production) return "";

  const hash = toHexString(await crypto.subtle.digest("MD5", new TextEncoder().encode(content)));
  return hash.slice(0, 8);
};

const toHexString = (bytes: ArrayBuffer): string => {
  return new Uint8Array(bytes).reduce((str, byte) => str + byte.toString(16).padStart(2, "0"), "");
};
