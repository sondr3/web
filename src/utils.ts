import { parse } from "std/path/parse.ts";
import { encodeHex } from "std/encoding/hex.ts";
import { crypto } from "std/crypto/mod.ts";
import * as path from "std/path/mod.ts";

export const digestFilename = async (filename: string, content: string): Promise<string> => {
  const digest = await crypto.subtle.digest("MD5", new TextEncoder().encode(content));
  const hash = encodeHex(digest).slice(0, 8);
  const path = parse(filename);

  return `${path.name}.${hash}${path.ext}`;
};

export const stripPrefix = (prefix: string, filePath: string): string => {
  const common = path.common([filePath, prefix]);
  return filePath.slice(common.length);
};

export const firstFilename = ({ paths }: Deno.FsEvent): string => {
  if (paths.length === 0) {
    throw new Error("No paths in event");
  }

  return path.parse(paths[0]).base;
};
