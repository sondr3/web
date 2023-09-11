import { ensureDir } from "std/fs/ensure_dir.ts";
import { Asset, PublicFile } from "./asset.ts";
import { stripPrefix } from "./utils.ts";
import * as path from "std/path/mod.ts";
import { PATHS } from "./constants.ts";
import { copy } from "std/fs/copy.ts";
import { Content, renderContent } from "./content.ts";
import { Mode } from "./context.ts";
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
    Array.from(assets.values()).map(async (asset) => {
      const out = path.join(PATHS.out, asset.filename);
      await ensureDir(path.dirname(out));
      await Deno.writeTextFile(out, asset.content);
    }),
  );
};

export const buildPages = async (
  pages: Map<string, Content>,
  mode: Mode,
  assets: Map<string, Asset>,
): Promise<void> => {
  await Promise.allSettled(
    Array.from(pages.values()).map(async (page) => {
      const out = path.join(PATHS.out, page.outPath);
      await ensureDir(path.dirname(out));
      let rendered = renderContent(page, assets);

      if (mode === "prod") {
        rendered = await minifyHTML(rendered);
      }

      await Deno.writeTextFile(out, rendered);
    }),
  );
};
