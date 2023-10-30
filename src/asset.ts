import * as sass from "sass";
import { ensureDir } from "std/fs/ensure_dir.ts";
import * as path from "std/path/mod.ts";
import { minify } from "terser";
import { minifyCSS } from "./minify.ts";
import { Path } from "./path.ts";
import { Mode, Site } from "./site.ts";
import { WriteFromSite } from "./writeable.ts";

export interface StaticAsset {
  path: Path;
  prefix: string;
}

export interface Asset extends WriteFromSite {
  source: Path;
  dest: Path;
  write(site: Site): Promise<void>;
}

abstract class AssetBase implements Asset {
  source: Path;
  dest: Path;

  public constructor(source: Path | string, dest: Path | string) {
    this.source = typeof source === "string" ? new Path(source) : source;
    this.dest = typeof dest === "string" ? new Path(dest) : dest;
  }

  abstract build(mode: Mode): Promise<string>;

  public async write(site: Site) {
    const content = await this.build(site.mode);
    await ensureDir(path.dirname(this.dest.absolute));
    await Deno.writeTextFile(this.dest.absolute, content);
  }
}

export class JavaScriptAsset extends AssetBase {
  async build(mode: Mode): Promise<string> {
    const src = await Deno.readTextFile(this.source.absolute);

    if (mode === "dev") {
      return src;
    }

    await this.dest.digest(src);
    const minified = await minify(src);

    if (!minified.code) {
      throw new Error("Could not minify JS");
    }

    return minified.code;
  }
}

export class CssAsset extends AssetBase {
  async build(mode: Mode): Promise<string> {
    const res = sass.compile(this.source.absolute);

    if (mode === "dev") {
      return res.css;
    }

    await this.dest.digest(res.css);
    return minifyCSS(res.css);
  }
}

// export class Asset implements WriteFromSite {
//   public path: Path;
//   public content: string;

//   private constructor(path: Path, content: string) {
//     this.path = path;
//     this.content = content;
//   }

//   public async write(_site: Site) {
//     const out = path.join(PATHS.out, this.path.filename);
//     await ensureDir(path.dirname(out));
//     await Deno.writeTextFile(out, this.content);
//   }

//   static async fromPath(path: string): Promise<Asset> {
//     const content = await Deno.readTextFile(path);

//     return new Asset(new Path(path), content);
//   }

//   static async buildCSS(path: string, mode: Mode): Promise<Asset> {
//     const outPath = new Path(path);
//     const res = sass.compile(path);

//     if (mode === "dev") {
//       return new Asset(outPath, res.css);
//     }

//     await outPath.digest(res.css);
//     const minified = minifyCSS(res.css);

//     return new Asset(outPath, minified);
//   }
// }
