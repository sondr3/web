import * as path from "std/path/mod.ts";

export const firstFilename = ({ paths }: Deno.FsEvent): string => {
  if (paths.length === 0) {
    throw new Error("No paths in event");
  }

  return path.parse(paths[0]).base;
};
