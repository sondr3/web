import path, { parse } from "node:path";

import { Context } from "../context.js";
import { fourOhFour } from "../templates/404.js";
import { landing } from "../templates/landing.js";
import { renderLayout, renderSpecial } from "../templates/templates.js";
import { createDirectory, writeFile } from "../utils/fs.js";
import { copyAssets, renderStyles } from "./assets.js";
import { compress } from "./compress.js";
import { buildPages, buildPosts, Content, decodeFrontmatter } from "./content.js";
import { sitemap } from "./sitemap.js";

export const build = async (ctx: Context): Promise<Error | void> => {
  await createDirectory(ctx.config.out);
  await copyAssets(ctx);
  await renderStyles(ctx);
  await renderPages(ctx);
  await renderSpecialPages(ctx);
  await sitemap(ctx);
  await compress(ctx);
};

export const renderPages = async (ctx: Context): Promise<Error | void> => {
  const { site, config } = ctx;
  await buildPages(ctx);
  await buildPosts(ctx);
  await Promise.allSettled(
    site.content().map(async (page: Content) => {
      const dir = parse(page.path());

      const res = await Promise.allSettled([
        createDirectory(path.join(config.out, dir.dir)),
        writeFile(path.join(config.out, page.path()), renderLayout(ctx, page)),
      ]);

      if (res.some((r) => r instanceof Error)) {
        return new Error(`Failed to render ${page.path()}`);
      }

      return;
    }),
  );
};

export const renderSpecialPages = async (ctx: Context): Promise<Error | void> => {
  const { config, site } = ctx;
  const index = new Content(
    { layout: "page" },
    decodeFrontmatter({
      doctitle: "Home",
      description: "The online home for Sondre Nilsen",
      slug: "",
    }),
    landing,
  );
  const indexRes = await writeFile(path.join(config.out, "index.html"), renderSpecial(ctx, index));
  if (indexRes instanceof Error) return indexRes;
  site.addPage(index);

  const missed = new Content(
    { layout: "page" },
    decodeFrontmatter({
      doctitle: "Not found",
      description: "You found... nothing?",
      slug: "404",
    }),
    fourOhFour,
  );
  await createDirectory(path.join(config.out, "404"));
  const missedRes = await writeFile(
    path.join(config.out, "404/index.html"),
    renderSpecial(ctx, missed),
  );
  if (missedRes instanceof Error) return missedRes;
  site.addPage(missed);
};
