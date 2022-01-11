import path, { parse } from "node:path";
import { EitherAsync } from "purify-ts/EitherAsync.js";

import { Asciidoc } from "./asciidoc.js";
import { buildPages, Content } from "./content.js";
import { createDirectory, writeFile } from "./fs.js";
import { Site } from "./site.js";
import { layout } from "./templates/layout.js";

export const renderPages = (site: Site, asciidoc: Asciidoc): EitherAsync<Error, void> =>
  EitherAsync(async () => {
    await buildPages(site, asciidoc).run();
    await Promise.allSettled(
      site.pages.map(async (page: Content) => {
        const dir = parse(page.metadata.path);

        await EitherAsync.sequence([
          createDirectory(path.join(site.config.out, dir.dir)),
          writeFile(
            path.join(site.config.out, page.metadata.path),
            layout(page.frontmatter.doctitle, page.document.getContent()),
          ),
        ]);
      }),
    );
  });
