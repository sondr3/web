import { parse } from "std/path/parse.ts";
import { Paths } from "./constants.ts";
import { Mode } from "./site.ts";
import * as sass from "sass";
import { digestFilename } from "./utils.ts";
import { minifyCSS } from "./minify.ts";

export interface StaticAsset {
  path: string;
  prefix: string;
}

export class Asset {
  public filename: string;
  public content: string;

  private constructor(filename: string, content: string) {
    this.filename = filename;
    this.content = content;
  }

  static async fromPath(path: string): Promise<Asset> {
    const content = await Deno.readTextFile(path);
    const filename = parse(path).base;

    return new Asset(filename, content);
  }

  static async buildCSS(paths: Paths, mode: Mode): Promise<Asset> {
    const outPath = `styles.css`;
    const res = sass.compile(`${paths.styles}/styles.scss`);

    if (mode === "dev") {
      return { filename: outPath, content: res.css };
    }

    const digest = await digestFilename(outPath, res.css);
    const minified = minifyCSS(res.css);

    return new Asset(digest, minified);
  }
}
