import { promises as fs } from "node:fs";
import path, { parse } from "node:path";

import { Context } from "../context.js";
import { Duration } from "../utils/duration.js";
import * as logger from "../utils/logger.js";
import { copyAssets, renderStyles } from "./assets.js";
import { compress } from "./compress.js";
import { buildPages, buildPosts, Content, decodeFrontmatter } from "./content.js";
import { sitemap } from "./sitemap.js";

export const build = async (ctx: Context): Promise<void> => {
  const buildDuration = new Duration();
  await fs.mkdir(ctx.config.out, { recursive: true });
  await copyAssets(ctx);
  await renderStyles(ctx);
  await renderPages(ctx);
  await renderSpecialPages(ctx);
  await sitemap(ctx);
  await compress(ctx);
  buildDuration.end();
  logger.info(`Finished building website`, buildDuration.result());
};

export const renderPages = async (ctx: Context): Promise<void> => {
  const { site, config, template } = ctx;
  await buildPages(ctx);
  await buildPosts(ctx);
  await Promise.allSettled(
    site.content().map(async (page: Content) => {
      const dir = parse(page.path());

      return Promise.allSettled([
        fs.mkdir(path.join(config.out, dir.dir), { recursive: true }),
        fs.writeFile(path.join(config.out, page.path()), template.render(page, ctx)),
      ]);
    }),
  );
};

export const renderSpecialPages = async (ctx: Context): Promise<void> => {
  const { config, site, template } = ctx;
  const index = new Content(
    { layout: "landing" },
    decodeFrontmatter({
      doctitle: "Home",
      description: "The online home for Sondre Nilsen",
      slug: "",
    }),
    "",
  );
  await fs.writeFile(path.join(config.out, "index.html"), template.render(index, ctx));
  site.addPage(index);

  const missed = new Content(
    { layout: "404" },
    decodeFrontmatter({
      doctitle: "Not found",
      description: "You found... nothing?",
      slug: "404",
    }),
    "",
  );
  await fs.mkdir(path.join(config.out, "404"), { recursive: true });
  await fs.writeFile(path.join(config.out, "404/index.html"), template.render(missed, ctx));
  site.addPage(missed);

  if (!config.production) {
    const resume = new Content(
      { layout: "resume" },
      decodeFrontmatter({
        doctitle: "Resume",
        description: "Resume for Sondre Nilsen",
        slug: "resume",
        draft: "true",
      }),
      "",
    );
    await fs.mkdir(path.join(config.out, "resume"), { recursive: true });
    await fs.writeFile(path.join(config.out, "resume/index.html"), template.render(resume, ctx));
    site.addPage(resume);
  }
};
