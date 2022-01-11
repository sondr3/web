import path, { parse } from "node:path";

import { landing } from "../templates/landing.js";
import { renderLayout, renderSpecial } from "../templates/templates.js";
import { createDirectory, writeFile } from "../utils/fs.js";
import { Asciidoc } from "./asciidoc.js";
import { copyAssets, renderStyles } from "./assets.js";
import { buildPages, Content } from "./content.js";
import { Site } from "./site.js";

export const build = async (site: Site, asciidoc: Asciidoc): Promise<Error | void> => {
  await createDirectory(site.config.out)
    .then(() => copyAssets(site))
    .then(() => renderStyles(site))
    .then(() => renderPages(site, asciidoc))
    .then(() => renderSpecialPages(site));
};

export const renderPages = async (site: Site, asciidoc: Asciidoc): Promise<Error | void> => {
  await buildPages(site, asciidoc);
  await Promise.allSettled(
    site.pages.map(async (page: Content) => {
      const dir = parse(page.path());

      const res = await Promise.all([
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
    {
      title: "Home => Eons :: IO ()",
      description: "The online home for Sondre Nilsen",
    },
    landing,
  );
  const res = await writeFile(path.join(site.config.out, "index.html"), renderSpecial(site, index));
  if (res instanceof Error) return res;
  site.addPage(index);
};
