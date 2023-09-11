import { parse } from "std/path/parse.ts";
import { Asset, PublicFile } from "./asset.ts";
import { PATHS, Paths } from "./constants.ts";
import { Content, contentFromPath } from "./content.ts";

export type Mode = "prod" | "dev";

export interface Metadata {
  url: string;
  out: string;
}

export interface Context {
  metadata: Metadata;
  mode: Mode;
  assets: Map<string, Asset>;
  pages: Map<string, Content>;
  public_files: Array<PublicFile>;
}

export const create_metadata = (mode: Mode): Metadata => {
  return {
    url: mode == "prod" ? "https://www.eons.io" : "http://localhost:3000",
    out: PATHS.out,
  };
};

const collectPages = async (paths: Paths): Promise<Array<Content>> => {
  const pages = new Array<Content>();

  for await (const entry of Deno.readDir(paths.pages)) {
    console.log(entry);
    const path = `${paths.pages}/${entry.name}`;
    const content = await contentFromPath(path, "page");
    pages.push(content);
  }

  return pages;
};

export const createContext = async (paths: Paths, mode: Mode): Promise<Context> => {
  const metadata = create_metadata(mode);
  const assets = new Map<string, Asset>();
  const pages = new Map<string, Content>();
  const public_files = new Array<PublicFile>();

  const collectedPages = await Promise.allSettled([collectPages(paths)]);

  (await collectPages(paths)).forEach((page) => {
    pages.set(parse(page.source).name, page);
  });

  return {
    metadata,
    mode,
    assets,
    pages,
    public_files,
  };
};
