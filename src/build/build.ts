import path, { parse } from "node:path";

import { fourOhFour } from "../templates/404.js";
import { landing } from "../templates/landing.js";
import { renderLayout, renderSpecial } from "../templates/templates.js";
import { createDirectory, writeFile } from "../utils/fs.js";
import { Asciidoc } from "./asciidoc.js";
import { copyAssets, renderStyles } from "./assets.js";
import { compress } from "./compress.js";
import { buildPages, Content, decodeFrontmatter } from "./content.js";
import { Site } from "./site.js";
import { sitemap } from "./sitemap.js";

export const build = async (site: Site, asciidoc: Asciidoc): Promise<Error | void> => {
  await createDirectory(site.config.out);
  await copyAssets(site);
  await renderStyles(site);
  await renderPages(site, asciidoc);
  await renderSpecialPages(site);
  await sitemap(site);
  await compress(site.config);
};

export const renderPages = async (site: Site, asciidoc: Asciidoc): Promise<Error | void> => {
  await buildPages(site, asciidoc);
  await Promise.allSettled(
    site.pages.map(async (page: Content) => {
      const dir = parse(page.path());

      const res = await Promise.allSettled([
        createDirectory(path.join(site.config.out, dir.dir)),
        writeFile(path.join(site.config.out, page.path()), renderLayout(site, page)),
      ]);

      if (res.some((r) => r instanceof Error)) {
        return new Error(`Failed to render ${page.path()}`);
      }

      return;
    }),
  );
};

export const renderSpecialPages = async (site: Site): Promise<Error | void> => {
  const index = new Content(
    { layout: "page" },
    decodeFrontmatter({
      doctitle: "Home",
      description: "The online home for Sondre Nilsen",
      slug: "",
    }),
    landing,
  );
  const indexRes = await writeFile(
    path.join(site.config.out, "index.html"),
    renderSpecial(site, index),
  );
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
  await createDirectory(path.join(site.config.out, "404"));
  const missedRes = await writeFile(
    path.join(site.config.out, "404/index.html"),
    renderSpecial(site, missed),
  );
  if (missedRes instanceof Error) return missedRes;
  site.addPage(missed);
};
