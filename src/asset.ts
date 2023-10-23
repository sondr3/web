import { parse } from "std/path/parse.ts";
import { PATHS } from "./constants.ts";
import { Mode, Site } from "./site.ts";
import * as sass from "sass";
import { digestFilename } from "./utils.ts";
import { minifyCSS } from "./minify.ts";
import * as path from "std/path/mod.ts";
import { ensureDir } from "std/fs/ensure_dir.ts";
import { WriteFromSite } from "./writeable.ts";

export interface StaticAsset {
  path: string;
  prefix: string;
}

export class Asset implements WriteFromSite {
  public filename: string;
  public content: string;

  private constructor(filename: string, content: string) {
    this.filename = filename;
    this.content = content;
  }

  public async write(_site: Site) {
    const out = path.join(PATHS.out, this.filename);
    await ensureDir(path.dirname(out));
    await Deno.writeTextFile(out, this.content);
  }

  static async fromPath(path: string): Promise<Asset> {
    const content = await Deno.readTextFile(path);
    const filename = parse(path).base;

    return new Asset(filename, content);
  }

  static async buildCSS(mode: Mode): Promise<Asset> {
    const outPath = `styles.css`;
    const res = sass.compile(`${PATHS.styles}/styles.scss`);

    if (mode === "dev") {
      return new Asset(outPath, res.css);
    }

    const digest = await digestFilename(outPath, res.css);
    const minified = minifyCSS(res.css);

    return new Asset(digest, minified);
  }
}
