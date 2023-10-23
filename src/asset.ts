import { PATHS } from "./constants.ts";
import { Mode, Site } from "./site.ts";
import * as sass from "sass";
import { minifyCSS } from "./minify.ts";
import * as path from "std/path/mod.ts";
import { ensureDir } from "std/fs/ensure_dir.ts";
import { WriteFromSite } from "./writeable.ts";
import { Path } from "./path.ts";

export interface StaticAsset {
  path: Path;
  prefix: string;
}

export class Asset implements WriteFromSite {
  public path: Path;
  public content: string;

  private constructor(path: Path, content: string) {
    this.path = path;
    this.content = content;
  }

  public async write(_site: Site) {
    const out = path.join(PATHS.out, this.path.filename);
    await ensureDir(path.dirname(out));
    await Deno.writeTextFile(out, this.content);
  }

  static async fromPath(path: string): Promise<Asset> {
    console.log(path);
    const content = await Deno.readTextFile(path);

    return new Asset(new Path(path), content);
  }

  static async buildCSS(mode: Mode): Promise<Asset> {
    const outPath = new Path(path.join(PATHS.out, `styles.css`));
    const res = sass.compile(`${PATHS.styles}/styles.scss`);

    if (mode === "dev") {
      return new Asset(outPath, res.css);
    }

    await outPath.digest(res.css);
    const minified = minifyCSS(res.css);

    return new Asset(outPath, minified);
  }
}
