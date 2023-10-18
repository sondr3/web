import { parse } from "std/path/parse.ts";
import { Asset, assetFromPath, buildCSS, PublicFile } from "./asset.ts";
import { PATHS, Paths } from "./constants.ts";
import { Content, contentFromPath } from "./content.ts";
import { walk } from "std/fs/walk.ts";
import { asyncToArray } from "./utils.ts";

export type Mode = "prod" | "dev";

export interface Metadata {
  url: URL;
  out: string;
  mode: Mode;
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
    url: new URL(mode == "prod" ? "https://www.eons.io" : "http://localhost:3000"),
    out: PATHS.out,
    mode,
  };
};

async function* collectJs(paths: Paths): AsyncGenerator<Asset> {
  for await (const entry of Deno.readDir(paths.js)) {
    const path = `${paths.js}/${entry.name}`;
    yield assetFromPath(path);
  }
}

export const collectPages = async (paths: Paths): Promise<Map<string, Content>> => {
  const pages = new Array<Content>();

  for await (const entry of Deno.readDir(paths.pages)) {
    const path = `${paths.pages}/${entry.name}`;
    const content = await contentFromPath(path, "page");
    pages.push(content);
  }

  return new Map(pages.flat().map((page) => [parse(page.source).name, page]));
};

const collectPublicFiles = async (paths: Paths): Promise<Array<PublicFile>> => {
  const public_files = new Array<PublicFile>();
  for await (const entry of walk(paths.public, { includeDirs: false })) {
    public_files.push({ path: entry.path, prefix: paths.public });
  }

  return public_files;
};

export const createContext = async (paths: Paths, mode: Mode): Promise<Context> => {
  const metadata = create_metadata(mode);
  const assets = new Map<string, Asset>();
  const public_files = await collectPublicFiles(paths);

  assets.set("styles.css", await buildCSS(paths, mode));
  (await asyncToArray(collectJs(paths))).map((a) => assets.set(a.filename, a));

  const pages = await collectPages(paths);

  return {
    metadata,
    mode,
    assets,
    pages,
    public_files,
  };
};
