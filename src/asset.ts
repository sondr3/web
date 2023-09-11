import { parse } from "std/path/parse.ts";
import { Paths } from "./constants.ts";
import { Mode } from "./context.ts";
import * as sass from "sass";
import { digestFilename } from "./utils.ts";
import { minifyCSS } from "./minify.ts";

export interface PublicFile {
  path: string;
  prefix: string;
}

export interface Asset {
  filename: string;
  content: string;
}

export const assetFromPath = async (path: string): Promise<Asset> => {
  const content = await Deno.readTextFile(path);
  const filename = parse(path).base;

  return { filename, content };
};

export const buildCSS = async (paths: Paths, mode: Mode): Promise<Asset> => {
  const outPath = `styles.css`;
  const res = sass.compile(`${paths.styles}/styles.scss`);

  if (mode === "dev") {
    return { filename: outPath, content: res.css };
  }

  const digest = await digestFilename(outPath, res.css);
  const minified = minifyCSS(res.css);

  return { filename: digest, content: minified };
};
