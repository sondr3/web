import { ensureDir } from "std/fs/ensure_dir.ts";
import { copy } from "std/fs/mod.ts";
import { walk } from "std/fs/walk.ts";
import * as log from "std/log/mod.ts";
import * as path from "std/path/mod.ts";
import { Asset, CssAsset, JavaScriptAsset, StaticAsset } from "./asset.ts";
import { JS_FILES, PATHS, SCSS_FILES } from "./constants.ts";
import { Content } from "./content.ts";
import { Path } from "./path.ts";
import { urlEntry, write_sitemap } from "./sitemap.ts";
import { write } from "./writeable.ts";
import { fromAsyncIterable } from "./utils.ts";

const logger = log.getLogger();

export type Mode = "prod" | "dev";

export class Site {
  public assets: Map<string, Asset> = new Map();
  public content: Map<string, Content> = new Map();
  public staticFiles: Array<StaticAsset> = [];
  public mode: Mode;
  public url: URL;

  private constructor(mode: Mode) {
    this.mode = mode;
    this.url = new URL(mode == "prod" ? "https://www.eons.io" : "http://localhost:3000");
  }

  public static async create(mode: Mode): Promise<Site> {
    const site = new Site(mode);

    const start = performance.now();

    await site.collectAssets();
    await fromAsyncIterable(site.collectCSS());
    await site.collectStaticFiles();
    await site.collectContents();

    const end = performance.now();
    logger.info(`Site creation took ${(end - start).toFixed(0)}ms`);

    return site;
  }

  public get isProd(): boolean {
    return this.mode === "prod";
  }

  public get isDev(): boolean {
    return this.mode === "dev";
  }

  public async write(): Promise<void> {
    await this.copyStaticAssets();
    await this.writeAssets();
    await this.writeContent();
    await this.writeSitemap();
  }

  public async *collectCSS(): AsyncGenerator<Asset> {
    for await (const entry of Object.values(SCSS_FILES)) {
      const asset = new CssAsset(entry.source, entry.dest);
      this.assets.set(entry.dest.filename, asset);
      yield asset;
    }
  }

  public async writeAssets(): Promise<void> {
    return await write(this.assets, this);
  }

  public async collectAssets(): Promise<void> {
    for await (const entry of Object.values(JS_FILES)) {
      const asset = new JavaScriptAsset(entry.source, entry.dest);
      this.collectAsset(asset);
    }
  }

  public collectAsset(asset: Asset): void {
    this.assets.set(asset.source.filename, asset);
  }

  public async collectStaticFiles(): Promise<void> {
    for await (const entry of walk(PATHS.public, { includeDirs: false })) {
      this.staticFiles.push({ path: new Path(entry.path), prefix: PATHS.public });
    }
  }

  public collectContent(content: Content): void {
    this.content.set(content.sourcePath.filename, content);
  }

  public async writeContent(): Promise<void> {
    return await write(this.content, this);
  }

  public async collectContents(): Promise<void> {
    for await (const entry of Deno.readDir(PATHS.pages)) {
      const path = `${PATHS.pages}/${entry.name}`;
      const content = await Content.fromPath(path, "page", this.url);

      if (content.frontmatter.draft && this.isProd) continue;

      this.collectContent(content);
    }
  }

  public copyStaticAssets = async (): Promise<void> => {
    await Promise.allSettled(this.staticFiles.map(async (file) => {
      const out = path.join(PATHS.out, file.path.common(file.prefix));
      await ensureDir(path.dirname(out));
      await copy(file.path.absolute, out, { overwrite: true });
    }));
  };

  public async writeSitemap(): Promise<void> {
    const entries = Array.from(this.content.values())
      .filter((p) => !p.frontmatter.special)
      .map((page) => urlEntry(page));

    await write_sitemap(entries, this);
  }
}
