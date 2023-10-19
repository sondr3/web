import { parse } from "std/path/parse.ts";
import { Asset, StaticAsset } from "./asset.ts";
import { PATHS } from "./constants.ts";
import { Content, contentFromPath } from "./content.ts";
import { walk } from "std/fs/walk.ts";

export type Mode = "prod" | "dev";

export class Site {
  public assets: Map<string, Asset> = new Map();
  public pages: Map<string, Content> = new Map();
  public staticFiles: Array<StaticAsset> = [];
  public mode: Mode;
  public url: URL;

  private constructor(mode: Mode) {
    this.mode = mode;
    this.url = new URL(mode == "prod" ? "https://www.eons.io" : "http://localhost:3000");
  }

  public static async build(mode: Mode): Promise<Site> {
    const site = new Site(mode);

    await site.collectAssets();
    await site.collectCSS();
    await site.collectStaticFiles();
    await site.collectPages();

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

  public async collectPages(): Promise<void> {
    for await (const entry of Deno.readDir(PATHS.pages)) {
      const path = `${PATHS.pages}/${entry.name}`;
      const content = await contentFromPath(path, "page");
      this.pages.set(parse(content.source).name, content);
    }
  }
}
