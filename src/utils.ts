import * as path from "std/path/mod.ts";

export const firstFilename = ({ paths }: Deno.FsEvent): string => {
  if (paths.length === 0) {
    throw new Error("No paths in event");
  }

  return path.parse(paths[0]).base;
};

export async function fromAsyncIterable<T>(gen: AsyncIterable<T>): Promise<Array<T>> {
  const out: Array<T> = [];
  for await (const x of gen) {
    out.push(x);
  }
  return out;
}
