import { ensureDir } from "std/fs/ensure_dir.ts";
import { Asset, PublicFile } from "./asset.ts";
import { stripPrefix } from "./utils.ts";
import * as path from "std/path/mod.ts";
import { PATHS } from "./constants.ts";
import { copy } from "std/fs/copy.ts";
import { Content, renderContent } from "./content.ts";
import { Context } from "./context.ts";
import { minifyHTML } from "./minify.ts";

export const copyPublicFiles = async (files: Array<PublicFile>): Promise<void> => {
  await Promise.allSettled(files.map(async (file) => {
    const out = path.join(PATHS.out, stripPrefix(file.prefix, file.path));
    await ensureDir(path.dirname(out));
    await copy(file.path, out, { overwrite: true });
  }));
};

export const writeAssets = async (assets: Map<string, Asset>): Promise<void> => {
  await Promise.allSettled(
    Array.from(assets.values()).map(async (asset) => await writeAsset(asset)),
  );
};

export const writeAsset = async (asset: Asset): Promise<void> => {
  const out = path.join(PATHS.out, asset.filename);
  await ensureDir(path.dirname(out));
  await Deno.writeTextFile(out, asset.content);
};

export const buildPages = async (
  pages: Map<string, Content>,
  context: Context,
): Promise<void> => {
  await Promise.allSettled(
    Array.from(pages.values()).map(async (page) => await buildPage(page, context)),
  );
};

export const buildPage = async (page: Content, context: Context): Promise<void> => {
  const out = path.join(PATHS.out, page.outPath);
  await ensureDir(path.dirname(out));
  let rendered = renderContent(page, context);

  if (context.metadata.mode === "prod") {
    rendered = await minifyHTML(rendered);
  }

  await Deno.writeTextFile(out, rendered);
};
