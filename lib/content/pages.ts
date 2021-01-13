import path from "path"
import { EitherAsync } from "purify-ts/EitherAsync"

import { Asciidoc, BuildError } from "../build"
import { Site } from "../site"
import * as pages from "../templates/pages"
import { FSError, readFile, walkDirectory } from "../utils"
import { renderContent, writeContent, writeHTML } from "."

/**
 * Render all pages in the `pages` directory in {@link config}.
 *
 * @param site - Build configuration
 * @param asciidoc - Asciidoctor instance
 * @param production - Whether to optimize output
 */
export const renderPages = (site: Site, asciidoc: Asciidoc, production: boolean): EitherAsync<FSError, void> =>
  EitherAsync(async () => {
    const pages = await walkDirectory(path.resolve(process.cwd(), site.config.content.pages), "adoc", false)

    for (const page of pages) {
      await readFile(page)
        .mapLeft((error) => new BuildError(error.message))
        .map(async (content) => {
          const document = renderContent(asciidoc, content, { layout: "page", path: `/${path.parse(page).name}/` })
          const rendered = asciidoc.render(site, document)

          site.addPage(document)
          const file = path.parse(page)
          await writeContent(path.resolve(site.config.out, file.name), writeHTML(rendered, production)).run()
        })
        .run()
    }
  })

/**
 * Renders "special" pages, e.g. landing page, 404 and such.
 *
 * @param site - Build configuration
 * @param production - Whether to optimize output
 */
export const renderSpecialPages = (site: Site, production: boolean): EitherAsync<Error, void> =>
  EitherAsync(async () => {
    await writeContent(site.config.out, writeHTML(pages.landing(site), production))
    site.addPage({
      metadata: {
        path: "/",
        layout: "default",
      },
      frontmatter: {
        title: "Eons",
        description: "Webpage for Sondre Nilsen",
        createdAt: new Date("2020-12-18"),
      },
    })

    await writeContent(path.resolve(site.config.out, "404/"), writeHTML(pages.notFound(site), production))
    site.addPage({
      metadata: {
        path: "/404/",
        layout: "default",
      },
      frontmatter: {
        title: "404",
        description: "Page not found",
        createdAt: new Date("2020-12-18"),
      },
    })
  })
