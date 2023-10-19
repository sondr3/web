import { parse } from "std/path/parse.ts";
import { Asset, StaticAsset } from "./asset.ts";
import { PATHS } from "./constants.ts";
import { Content, contentFromPath } from "./content.ts";
import { walk } from "std/fs/walk.ts";
import * as log from "std/log/mod.ts";

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
    await site.collectCSS();
    await site.collectStaticFiles();
    await site.collectContents();

    const end = performance.now();
    logger.info(`Site creation took ${(end - start).toFixed(0)}ms`);

    return site;
  }

  public isProd = (): boolean => this.mode === "prod";
  public isDev = (): boolean => this.mode === "dev";

  public async collectAssets(): Promise<void> {
    for await (const entry of Deno.readDir(PATHS.js)) {
      const path = `${PATHS.js}/${entry.name}`;
      const asset = await Asset.fromPath(path);
      this.assets.set(asset.filename, asset);
    }
  }

  public async collectCSS(): Promise<void> {
    this.assets.set("styles.css", await Asset.buildCSS(PATHS, this.mode));
  }

  public async collectStaticFiles(): Promise<void> {
    for await (const entry of walk(PATHS.public, { includeDirs: false })) {
      this.staticFiles.push({ path: entry.path, prefix: PATHS.public });
    }
  }

  public collectContent(content: Content): void {
    this.content.set(content.source, content);
  }

  public async collectContents(): Promise<void> {
    for await (const entry of Deno.readDir(PATHS.pages)) {
      const path = `${PATHS.pages}/${entry.name}`;
      const content = await contentFromPath(path, "page");
      this.collectContent(content);
    }
  }
}
