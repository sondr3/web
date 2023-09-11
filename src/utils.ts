import { parse } from "std/path/parse.ts";
import { crypto, toHashString } from "std/crypto/mod.ts";

export const digestFilename = async (filename: string, content: string): Promise<string> => {
  const digest = await crypto.subtle.digest("MD5", new TextEncoder().encode(content));
  const hash = toHashString(digest).slice(0, 8);
  const path = parse(filename);

  return `${path.name}.${hash}${path.ext}`;
};
