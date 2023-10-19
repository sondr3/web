import { ensureDir } from "std/fs/ensure_dir.ts";
import { Asset, StaticAsset } from "./asset.ts";
import { stripPrefix } from "./utils.ts";
import * as path from "std/path/mod.ts";
import { PATHS } from "./constants.ts";
import { copy } from "std/fs/copy.ts";
import { Content } from "./content.ts";
import { Site } from "./site.ts";

export const copyPublicFiles = async (files: Array<StaticAsset>): Promise<void> => {
  await Promise.allSettled(files.map(async (file) => {
    const out = path.join(PATHS.out, stripPrefix(file.prefix, file.path));
    await ensureDir(path.dirname(out));
    await copy(file.path, out, { overwrite: true });
  }));
};

export const writeAssets = async (assets: Map<string, Asset>): Promise<void> => {
  await Promise.allSettled(
    Array.from(assets.values()).map(async (asset) => await asset.write()),
  );
};

export const buildPages = async (
  pages: Map<string, Content>,
  site: Site,
): Promise<void> => {
  await Promise.allSettled(
    Array.from(pages.values()).map(async (page) => await page.write(site)),
  );
};
