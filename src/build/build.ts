import path, { parse } from "node:path";
import { EitherAsync } from "purify-ts/EitherAsync.js";

import { landing, layout } from "../templates/templates.js";
import { createDirectory, writeFile } from "../utils/fs.js";
import { Asciidoc } from "./asciidoc.js";
import { _renderStyle, copyAssets } from "./assets.js";
import { buildPages, Content, decodeFrontmatter } from "./content.js";
import { Site } from "./site.js";

export const build = (site: Site, asciidoc: Asciidoc): EitherAsync<Error, void> =>
  EitherAsync(async () => {
    await EitherAsync.sequence([
      createDirectory(site.config.out),
      copyAssets(site),
      _renderStyle(site),
      renderPages(site, asciidoc),
      renderSpecialPages(site),
    ])
      .mapLeft((e) => e)
      .run();
  });

export const renderPages = (site: Site, asciidoc: Asciidoc): EitherAsync<Error, void> =>
  EitherAsync(async () => {
    await buildPages(site, asciidoc).run();
    await Promise.allSettled(
      site.pages.map(async (page: Content) => {
        const dir = parse(page.path());

        await EitherAsync.sequence([
          createDirectory(path.join(site.config.out, dir.dir)),
          writeFile(path.join(site.config.out, page.path()), layout(page.title(), page.content())),
        ]).run();
      }),
    );
  });

export const renderSpecialPages = (site: Site): EitherAsync<Error, void> =>
  EitherAsync(async () => {
    await writeFile(path.join(site.config.out, "index.html"), landing());
    site.addPage(
      new Content(
        { layout: "page" },
        decodeFrontmatter({ doctitle: "Home", description: "Homepage" }),
        "",
      ),
    );
  });
